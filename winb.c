/*
 * winb kernel module
 * (C)2006 Dan Ponte. Under the 3-clause BSD license.
 * Portions taken from mbmon. (C)1999-2004  Yoshifumi R. Shimizu.
 * Builds under FreeBSD 6.1 and others
 */

#include <sys/types.h>
#include <sys/module.h>
#include <sys/systm.h>  /* uprintf */ 
#include <sys/errno.h>
#include <sys/param.h>  /* defines used in kernel.h */
#include <sys/kernel.h> /* types used in module initialization */
#include <sys/bus.h>
#include <machine/bus.h>
#include <machine/resource.h>
#include <sys/rman.h>
#include <sys/time.h>
#include <sys/sysctl.h>
#include <machine/cpufunc.h>

#include <isa/isavar.h>
#include "isa_if.h"

/* 
 * Load handler that deals with the loading and unloading of a KLD.
 */
#define IOP_ADDR      0x290
#define INDEX_REG_PORT (IOP_ADDR + 0x05)
#define DATA_PORT      (IOP_ADDR + 0x06)
#define WAIT outb(0xEB,0x00)

#define	LM75_ADDR_START		0x90	/* 0x90-0x9E (0x48-0x4F) */
#define	LM75_ADDR_END		0x9E
#define	WINBD_ADDR_START	0x50	/* 0x50-0x5E (0x28-0x2F) */
#define	WINBD_ADDR_END		0x5E
#define	ASUSM_ADDR_FIX		0xEE	/* ASUS Mozart-2, 0xEE (0x77) only */

#define	WINBD_CONFIG	0x40
#define	WINBD_SMBADDR	0x48
#define	WINBD_DEVCID	0x49
#define	WINBD_CHIPID	0x58
#define	WINBD_VENDEX	0x4E
#define	WINBD_VENDID	0x4F
#define	ANADM_VENDID	0x3E

/* temp nr=0,1,2; volt nr=0,...6; fan nr=0,1,2 */
#define	WINBD_TEMP0		0x27
#define	ASUSB_TEMP4		0x17
#define	ASUSM_TEMP2		0x13
#define	WINBD_TEMPADDR	0x4A
#define	WINBD_VOLT(nr)	(0x20 + (nr))
#define	WINBD_FAN(nr)	(0x28 + (nr))
#define	WINBD_FANDIV	0x47
#define	WINBD_REGPIN	0x4B
#define	ASUSM_FANDIV	0xA1
#define	ANADM_TEMPCFG	0x4B
#define WINB_INT_STAT2	0x42

#define	WINBD_DIOSEL	0x59
#define	WINBD_VMCTRL	0x5D

#define WINBD_chkRegNum 8
static int probed = 0;


/* Register checked for probing */


static int chkReg[] = {
	0x40, 0x41, 0x42, 0x43,
	0x44, 0x45, 0x46, 0x47,
	0x48, 0x49, 0x4A, 0x4B,
	0x4C, 0x4D, 0x4E, 0x4F,
	0x56, 0x58, 0x59, 0x5D,
	0x3E, 0x13, 0x17, 0xA1,
	0x20, 0x22, 0x23, 0x24,
	0x27, 0x29, 0x2A, 0x2B,
	-1 };

enum winbond_chips {
	NOSENSER,
	W83781D,
	W83782D,
	W83783S,
	W83791D,
	W83627HF,
	W83697HF,
	WBUNKNOWN,
	AS99127F,
	ASB100,
	ASM58,
	LM78,
	LM79,
	ADM9240,
	UNKNOWN
};

struct winb_softc {
	enum winbond_chips wbdchipid;
	enum winbond_chips wbdlmid;
	int temp1_flag;
	int temp2_flag;
	struct sysctl_ctx_list	sysctl_ctx;
	struct sysctl_oid	*winbroot;
	struct {
		struct sysctl_oid	*temp1, *temp2, *temp3;
		struct sysctl_oid	*fan1, *fan2, *fan3;
		struct sysctl_oid	*vc0, *vc1, *v33;
		struct sysctl_oid	*v50p, *v50n, *v12p, *v12n;
		struct sysctl_oid	*chassis;
		struct sysctl_oid	*f1duty, *f2duty;
		struct sysctl_oid	*pwrled;
	} oids;
};


static int Read(int addr)
{
	int ret;
	outb(INDEX_REG_PORT, addr); WAIT;
	ret = inb(DATA_PORT); WAIT;
	return (ret & 0xFF);
}

static void Write(int addr, int value)
{
	outb(INDEX_REG_PORT, addr); WAIT;
	outb(DATA_PORT, value); WAIT;
}

static int ReadTemp1(void)
{
	int ret;
	Write(0x4E, 0x01);	/* changing to bank 1 */
	ret = Read(0x50) | (Read(0x51) << 8);
	Write(0x4E, 0x00);	/* returning to bank 0 */
	return ret;
}

static int ReadTemp2(void)
{
	int ret;
	Write(0x4E, 0x02);	/* changing to bank 2 */
	ret = Read(0x50) | (Read(0x51) << 8);
	Write(0x4E, 0x00);	/* returning to bank 0 */
	return ret;
}

static int chkReg_Probe(int Reg[])
{
	int i, n, r, ret = 0;
	for(i = 0; (r = Reg[i]) != -1; i++) {
		n = Read(r);
		if(n != 0xFF)
			++ret;
	}
	return ret;
}

/*
 *	\retval	0xFFFF	no sensor
 *	\retval	other	temperature
 *  no = 0,1,2
 */
#define FLOATIT
#ifdef FLOATIT
static	int	winbond_temp(int no, struct winb_softc *wbc)
{
	int n = 0;

	if (no < 0 || 2 < no)
		return 0xFFFF;
	if (no == 2 &&
		(wbc->wbdchipid == W83783S || wbc->wbdchipid == W83697HF || wbc->wbdchipid == ASM58))
		return 0xFFFF;

	if (no == 0) {
		n = Read(WINBD_TEMP0);
		return n;
	} else if (no == 1) {
		if (!wbc->temp1_flag)
			n = ReadTemp1();
#ifdef SYRS
	} else if (no == 2 && !wbc->temp2_flag) {
#else
	} else if (no == 2) {
		if (wbc->wbdchipid == ASB100) {
		  if (!wbc->temp1_flag)
			n = ReadTemp1();
		} else if (!wbc->temp2_flag)
#endif
			n = ReadTemp2();
	}

	if ((n & 0xFF) >= 0x80)
		n = 0;
	return n;
}

static int winbond_chassis(device_t dev)
{
	int n;

	Write(0x4E, 0x04);	/* changing to bank 4 */
	n = Read(0x5A); /* RT hardware status II */

	if(n & 0x10 /* chassis bit */) {
		return 1;
	} else {
		return 0;
	}
	/* NOTREACHED */
}

static void winbond_setduty(device_t dev, int fan2 /* if true, fan2 set and not fan1 */, int duty_cycle /* byte */)
{
	Write(0x4E, 0x00); /* bank 0 */
	Write(0x5A + (fan2 ? 1 : 0), duty_cycle);
}

static int winbond_getduty(device_t dev, int fan2)
{
	Write(0x4E, 0x00); /* bank 0 */
	return Read(0x5A + (fan2 ? 1 : 0));
}

static void winbond_clearchas(void)
{
	int oldval;

	outb(0x2E, 0x87);
	outb(0x2E, 0x87); /* double write to enter "extended function mode" */
	outb(0x2E, 0x07); /* point to LD# reg */
	outb(0x2F, 0xA); /* select device number A */
	outb(0x2E, 0xE6); /* select CRE6 */
	oldval = inb(0x2F); /* get old value */
	outb(0x2E, 0xE6); /* select CRE6 */
	outb(0x2F, oldval | 0x40 /* status clear */); /* update CRE6 with whatever */
	outb(0x2E, 0xE6); /* select CRE6 */
	outb(0x2F, oldval); /* reclear that bit */
	outb(0x2E, 0xAA); /* write AAh to exit extended mode */
}

static int winbond_getpwrled(device_t dev)
{
	int val;

	outb(0x2E, 0x87);
	outb(0x2E, 0x87); /* double write to enter "extended function mode" */
	outb(0x2E, 0x07); /* point to LD# reg */
	outb(0x2F, 0x8); /* select device number 8 */
	outb(0x2E, 0xF5); /* select CRE6 */
	val = inb(0x2F); /* get old value */
	outb(0x2E, 0xAA); /* write AAh to exit extended mode */

	val >>= 6;
	return val;
}

static void winbond_setpwrled(device_t dev, int nv)
{
	int val, tor;

	tor = nv & 0x3; /* grab first two bits */
	tor <<= 6;

	outb(0x2E, 0x87);
	outb(0x2E, 0x87); /* double write to enter "extended function mode" */
	outb(0x2E, 0x07); /* point to LD# reg */
	outb(0x2F, 0x8); /* select device number 8 */
	outb(0x2E, 0xF5); /* select CRF5 */
	val = inb(0x2F); /* get old value */
	outb(0x2E, 0xF5); /* select CRF5 */
	outb(0x2F, (val & ~(0x3 << 6)) | tor); /* update CRE6 with whatever */
	outb(0x2E, 0xAA); /* write AAh to exit extended mode */

}

/*
 *	\retval	0x0000FFFF	no sensor
 *  no = 0,1,2,...,6
 */
static	int	winbond_volt(int no, struct winb_softc *wbc)
{
	int n;

	if (no < 0 || 6 < no)
		return 0xFFFF;
	if (wbc->wbdchipid == ASM58 && (no == 1 || no > 4))
		return 0xFFFF;
	if (wbc->wbdchipid == ADM9240 && (no > 5))
		return 0xFFFF;

	n = Read(WINBD_VOLT(no));

	return n;
}
#endif

/*
	Controlling Fan Divisor for 1st/2nd fans: CR = 0x47.
	1st two bits for fan1, 2nd two bits for fan2

         7     4 3     0
        +-+-+-+-+-+-+-+-+     xx = 00,01,10,11  div1fac = 1,2,4,8
        |y y|x x| VolID |     yy = 00,01,10,11  div2fac = 1,2,4,8
        +-+-+-+-+-+-+-+-+    initial values: xx=01, yy=01

	Controlling Fan Divisor for 3rd fan: CR = 0x4B.
	1st two bits for fan3

         7 6 5         0
        +-+-+-+-+-+-+-+-+
        |z z|           |     zz = 00,01,10,11  div3fac = 1,2,4,8
        +-+-+-+-+-+-+-+-+    initial values: zz=01

	3rd fan divisor available for Winbond (not for LM78/79).

    Bit 2 of Fan Divisor: CR = 0x5D (Winbond chips except 781D).

         7 6 5         0
        +-+-+-+-+-+-+-+-+
        |z|y|x|         |     x, y, z for bit 2 of fan 1, 2, 3
        +-+-+-+-+-+-+-+-+
 */

/*
 *	\retval	0xFFFF no sensor
 *  no = 0,1,2
 *
 *  Clock is 22.5kHz (22,500 x 60 = 1350000 counts/minute)
 */
static	int		winbond_fanrpm(int no, struct winb_softc *wbc)
{
	int r, n1 = 0x50, n2 = 0x40, n3 = 0x00;
	static int div[3] = {1,1,1};

	if (no < 0 || 2 < no)
		return 0xFFFF;
	if (no == 2
		&& (wbc->wbdchipid == W83697HF || wbc->wbdchipid == ASM58
			|| wbc->wbdchipid == ADM9240))
		return 0xFFFF;

	if (W83782D <= wbc->wbdchipid && wbc->wbdchipid <= W83697HF) {
		n3 = Read(WINBD_VMCTRL);	/* bit 2 */
	}
	if (no != 2) {
		n1 = Read(WINBD_FANDIV);	/* bit 0,1 */
		div[0] = ((n1 >> 4) & 0x03) | ((n3 & 0x20) >> 3);
		div[1] =  (n1 >> 6) | ((n3 & 0x40) >> 4);
	} else if (wbc->wbdchipid < LM78) {
		n2 = Read(WINBD_REGPIN);	/* bit 0,1 */
		div[2] =  (n2 >> 6) | ((n3 & 0x80) >> 5);
	}

	r = Read(WINBD_FAN(no));
	if (r == 0xFF) {
		/* change divisor for the sake of next call ! */
		if (no != 2) {
			if (div[no] < 3)
				++(div[no]);
			else
				div[no] = 0;
			r = (n1 & 0x0F) | ((div[0] & 0x03) << 4) | ((div[1] & 0x03) << 6);
			Write(WINBD_FANDIV, r);
		} else if (wbc->wbdchipid < LM78) {
			if (div[no] < 3)
				++(div[no]);
			else
				div[no] = 0;
			r = (n2 & 0x3F) | ((div[2] & 0x03) << 6);
			Write(WINBD_REGPIN, r);
		}
		if (W83782D <= wbc->wbdchipid && wbc->wbdchipid <= W83697HF) {
			r = (n3 & 0x1F) | ((div[0] & 0x04) << 3) |
				((div[1] & 0x04) << 4) | ((div[2] & 0x04) << 5);
			Write(WINBD_VMCTRL, r);
		}
		return 0xFFFF;
	} else if (r == 0) {
		return 0xFFFF;
	}

	return 1350000 / (r * (1 << div[no]));
}


static int
winb_loader(struct module *m, int what, void *arg)
{
  int err = 0;
  
  switch (what) {
  case MOD_LOAD:                /* kldload */
uprintf("winb loaded.\n");
break;
  case MOD_UNLOAD:
uprintf("winb unloaded.\n");
break;
  default:
err = EINVAL;
break;
  }
  return(err);
}

static void winb_isa_identify(driver_t *dr, device_t dev)
{
	struct winb_softc *wbc;
	wbc = device_get_softc(dev);
}

static int winb_isa_probe (device_t dev)
{
	int n, nd, nc, nvl, nvu, nvx, nva;
	struct winb_softc *wbc;
	wbc = device_get_softc(dev);
	if(probed)
		return ENXIO;

	/* probe it */
	if (chkReg_Probe(chkReg) < WINBD_chkRegNum)
		return ENXIO;

	nd = Read(WINBD_DEVCID) & 0xFE;
	nc = Read(WINBD_CHIPID);
	nvx = Read(WINBD_VENDEX);
	Write(WINBD_VENDEX, 0x00);
	nvl = Read(WINBD_VENDID);
	Write(WINBD_VENDEX, 0x80);
	nvu = Read(WINBD_VENDID);
	nva = Read(ANADM_VENDID);

	if (nvl == 0xA3 && nvu == 0x5C) {	/* Winbond Chip */
	  switch (nc & 0xFE) {
		case 0x10:	/* 0x10 (or 0x11) */
			wbc->wbdchipid = W83781D;
			break;
		case 0x20:	/* 0x20 (or 0x21) 627HF */
		case 0x90:	/* 0x90 (or 0x91?) 627THF */
		case 0x1A:	/* 0x1A (??)  627THF-A */
			wbc->wbdchipid = W83627HF;
			break;
		case 0x30:	/* 0x30 (or 0x31) */
			wbc->wbdchipid = W83782D;
			if (nc == 0x31)
				wbc->wbdchipid = AS99127F;	/* very special, but ... */
			break;
		case 0x40:	/* 0x40 (or 0x41) */
			wbc->wbdchipid = W83783S;
			break;
		case 0x60:	/* 0x60 (or 0x61) */
			wbc->wbdchipid = W83697HF;
			break;
		case 0x70:	/* 0x70 (or 0x71) */
			wbc->wbdchipid = W83791D;
			break;
		default:
			return ENXIO;
	  }
	}

	wbc->wbdlmid = wbc->wbdchipid;

	n = Read(WINBD_TEMPADDR);
	if (!(wbc->temp1_flag = (n & 0x08) >> 3)) {
		if (ReadTemp1() == 0xFFFF) {
			wbc->temp1_flag = 1;	/* disable! */
		}
	}

	if (!(wbc->temp2_flag = (n & 0x80) >> 7)) {
		if (ReadTemp2() == 0xFFFF) {
			wbc->temp2_flag = 1;	/* disable! */
		}
	}
	probed = 1;

	/* return wbc->wbdchipid or something */
	return 0;
}

#define WB_FAN_FNC(id, num)	static int winb_ ## id ## _sysc(SYSCTL_HANDLER_ARGS) \
	{ \
		struct winb_softc *wbc; \
		device_t dev; \
		int err, val; \
		dev = oidp->oid_arg1; \
		wbc = device_get_softc(dev); \
		val = winbond_fanrpm(num, wbc); \
		err = sysctl_handle_int(oidp, &val, sizeof(val), req); \
		if(err) \
			return err; \
		return 0; \
	}
#define WB_CHAS_FNC(id)		static int winb_ ## id ## _sysc(SYSCTL_HANDLER_ARGS) \
	{ \
		struct winb_softc *wbc; \
		device_t dev; \
		int err, val, oval; \
		dev = oidp->oid_arg1; \
		wbc = device_get_softc(dev); \
		oval = val = winbond_chassis(dev); \
		err = sysctl_handle_int(oidp, &val, sizeof(val), req); \
		if((val == 2) && (oval != 2)) { \
			winbond_clearchas(); \
			device_printf(dev, "supposedly cleared chassis bit\n"); \
		} \
		if(err) \
			return err; \
		return 0; \
	}
#define WB_PWRLED_FNC(id)		static int winb_ ## id ## _sysc(SYSCTL_HANDLER_ARGS) \
	{ \
		struct winb_softc *wbc; \
		device_t dev; \
		int err, val, oval; \
		dev = oidp->oid_arg1; \
		wbc = device_get_softc(dev); \
		oval = val = winbond_getpwrled(dev); \
		err = sysctl_handle_int(oidp, &val, sizeof(val), req); \
		if(val <= 3 && val >= 0 && (val != oval)) { \
			winbond_setpwrled(dev, val); \
		} else if(val != oval) { \
			return EPERM; \
		} \
		if(err) \
			return err; \
		return 0; \
	}

#define WB_FANDUTY_FNC(id, n)	static int winb_ ## id ## _sysc(SYSCTL_HANDLER_ARGS) \
	{ \
		struct winb_softc *wbc; \
		device_t dev; \
		int err, val, oval; \
		dev = oidp->oid_arg1; \
		wbc = device_get_softc(dev); \
		oval = val = winbond_getduty(dev, n); \
		err = sysctl_handle_int(oidp, &val, sizeof(val), req); \
		if(val != oval) { \
			winbond_setduty(dev, n, val); \
		} \
		if(err) \
			return err; \
		return 0; \
	}

#ifdef FLOATIT
#define WB_TEMP_FNC(id, num)	static int winb_ ## id ## _sysc(SYSCTL_HANDLER_ARGS) \
	{ \
		struct winb_softc *wbc; \
		device_t dev; \
		int err; \
		int val; \
		dev = oidp->oid_arg1; \
		wbc = device_get_softc(dev); \
		val = winbond_temp(num, wbc); \
		err = sysctl_handle_int(oidp, &val, sizeof(val), req); \
		if(err) \
			return err; \
		return 0; \
	}
#define WB_VOLT_FNC(id, num)	static int winb_ ## id ## _sysc(SYSCTL_HANDLER_ARGS) \
	{ \
		struct winb_softc *wbc; \
		device_t dev; \
		int err; \
		int val; \
		dev = oidp->oid_arg1; \
		wbc = device_get_softc(dev); \
		val = winbond_volt(num, wbc); \
		err = sysctl_handle_int(oidp, &val, sizeof(val), req); \
		if(err) \
			return err; \
		return 0; \
	}
/*  *vc0 = v[0], *vc1 = v[1];
 *          *v33 = v[2], *v50p = v[3], *v12p = v[4];
 *                  *v12n = v[5], *v50n = v[6];
 */
#endif

WB_FAN_FNC(fan1, 0)
WB_FAN_FNC(fan2, 1)
WB_FAN_FNC(fan3, 2)
WB_TEMP_FNC(temp1, 0)
WB_TEMP_FNC(temp2, 1)
WB_TEMP_FNC(temp3, 2)
WB_VOLT_FNC(vc0, 0)
WB_VOLT_FNC(vc1, 1)
WB_VOLT_FNC(v33, 2)
WB_VOLT_FNC(v50p, 3)
WB_VOLT_FNC(v12p, 4)
WB_VOLT_FNC(v12n, 5)
WB_VOLT_FNC(v50n, 6)
WB_CHAS_FNC(chassis)
WB_FANDUTY_FNC(f1duty, 0)
WB_FANDUTY_FNC(f2duty, 1)
WB_PWRLED_FNC(pwrled)

static int winb_isa_attach (device_t dev)
{
	int rc;
	struct winb_softc *wbc;


	wbc = device_get_softc(dev);
	rc = bus_set_resource(dev, SYS_RES_IOPORT, 0, WINBD_ADDR_START, WINBD_ADDR_END - WINBD_ADDR_START);
	if(rc != 0) {
		device_printf(dev, "set res error! rc %d\n", rc);
		return ENXIO;
	}
	sysctl_ctx_init(&wbc->sysctl_ctx);
	wbc->winbroot = SYSCTL_ADD_NODE(&wbc->sysctl_ctx, SYSCTL_STATIC_CHILDREN(_hw),
		              OID_AUTO, "winbond", CTLFLAG_RD, 0, "winbond monitoring");
#define ADDPR(oid,hand,type, t, desc)	wbc->oids.oid = SYSCTL_ADD_PROC(&wbc->sysctl_ctx, SYSCTL_CHILDREN(wbc->winbroot), OID_AUTO, #oid, CTLTYPE_ ## type | \
		CTLFLAG_RD, dev, sizeof(dev), hand, t, desc)
#define ADDPRW(oid,hand,type, t, desc)	wbc->oids.oid = SYSCTL_ADD_PROC(&wbc->sysctl_ctx, SYSCTL_CHILDREN(wbc->winbroot), OID_AUTO, #oid, CTLTYPE_ ## type | \
		CTLFLAG_RW, dev, sizeof(dev), hand, t, desc)
#define ADDUPR(oid,hand,type, t, desc)	wbc->oids.oid = SYSCTL_ADD_PROC(&wbc->sysctl_ctx, SYSCTL_CHILDREN(wbc->winbroot), OID_AUTO, #oid "_up", CTLTYPE_ ## type | \
		CTLFLAG_RD, dev, sizeof(dev), hand, t, desc)
	ADDPR(fan1, winb_fan1_sysc, INT, "I", "Fan1");
	ADDPR(fan2, winb_fan2_sysc, INT, "I", "Fan2");
	ADDPR(fan3, winb_fan3_sysc, INT, "I", "Fan3");
	ADDPRW(pwrled, winb_pwrled_sysc, INT, "I", "PWRLED");
	ADDPRW(chassis, winb_chassis_sysc, INT, "I", "Chassis");
	ADDPRW(f1duty, winb_f1duty_sysc, INT, "I", "Fan1Duty");
	ADDPRW(f2duty, winb_f2duty_sysc, INT, "I", "Fan2Duty");
	ADDUPR(temp1, winb_temp1_sysc, INT, "I", "Temp1");
	ADDUPR(temp2, winb_temp2_sysc, INT, "I", "Temp2");
	ADDUPR(temp3, winb_temp3_sysc, INT, "I", "Temp3");
	ADDUPR(vc0, winb_vc0_sysc, INT, "I", "VC0");
	ADDUPR(vc1, winb_vc1_sysc, INT, "I", "VC1");
	ADDUPR(v33, winb_v33_sysc, INT, "I", "V3.3");
	ADDUPR(v50p, winb_v50p_sysc, INT, "I", "V5.0+");
	ADDUPR(v50n, winb_v50n_sysc, INT, "I", "V5.0-");
	ADDUPR(v12p, winb_v12p_sysc, INT, "I", "V12+");
	ADDUPR(v12n, winb_v12n_sysc, INT, "I", "V12-");
#undef ADDPR


	return 0;
}

static int winb_isa_detach (device_t dev)
{
	struct winb_softc *wbc;
	wbc = device_get_softc(dev);
	if(sysctl_ctx_free(&wbc->sysctl_ctx)) {
		uprintf("can't free context");
		return ENOTEMPTY;
	} else {
		bus_delete_resource(dev, SYS_RES_IOPORT, 0);
		return 0;
	}
	/* NOTREACHED */
	return 0;
}


/* Declare this module to the rest of the kernel */
/* table of supported bus methods */
static device_method_t winb_isa_methods[] = {
    /* list all the bus method functions supported by the driver */
    /* omit the unsupported methods */
	DEVMETHOD(device_identify, winb_isa_identify),
    DEVMETHOD(device_probe,     winb_isa_probe),
    DEVMETHOD(device_attach,    winb_isa_attach),
    DEVMETHOD(device_detach,    winb_isa_detach),
	{ 0, 0 }
};


static driver_t winb_isa_driver = {
    "winb",
    winb_isa_methods,
    sizeof(struct winb_softc),
};


static devclass_t winb_devclass;

DRIVER_MODULE(winb, acpi, winb_isa_driver, winb_devclass, winb_loader, NULL);
