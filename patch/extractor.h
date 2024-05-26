/*** EXTRACTOR STUFF ***/

// Go to unixmain.c and slap #include "extractor.h" after all the other
// includes. This overwrites the main() function with the extractor stuff.

#include <stdio.h>

//#include "date.h"
static void extract_monsterdata_to_yaml(
        FILE* f
      , const char* variant
      , const char* command_prefix );
static const char* detect_variant( const char** command_prefix );

#ifdef G_SHEOL
#ifdef G_HELL
#ifdef G_NOHELL
#define HASSHEOL
#endif
#endif
#endif

#ifdef HASSHEOL
static boolean pb_prohibited_by_generation_flags(struct permonst *ptr
        , int inhell, int insheol);
#endif

#undef main

// UnNetHack (maybe other variants) have a function called "hates_silver"
// which determines if a monster is vulnerable to silver. Uncomment this if
// the function exists if you want pinobot to report about silver
// vulnerability.
//#define HAS_HATES_SILVER

// Has "mon_hates_material" function.
// As of writing of this, this is found in evilhack, splicehack and
// xnethack
//
// Used to determine silver-hating.
#define HAS_MON_HATES_MATERIAL

#define HAS_RACEBOOLEAN_BITFLAGS    // Has 'mhflags' in permonst
//#define HAS_MONST_GLOBALS_INIT
//#define GENDERED_NAMES   // Has pmnames instead of mname
//#define HAS_MONSTR       // has monstr[idx] instead of mons.difficulty
#define MONSYMS_IS_STRUCT  // def_monsyms is a structure, instead of an array of chars

//#define DNETHACK_MONFLAGS   // game uses dNetHack's refactor of monster flags

#define HAS_MNUM

//#define DNETHACK_ACS        // game has three different ACs for monsters, rather than one.
//#define HAS_DNETHACK_ID_PERMONST

#ifdef GENDERED_NAMES
#define is_valid_monster(str) ((str).pmnames[0] || (str).pmnames[1] || (str).pmnames[2])
#define get_gender_name(str, gender_idx) ((str).pmnames[gender_idx])
#define num_genders 3
#else
#define is_valid_monster(str) ((str).mname[0])
#define get_gender_name(str, gender_idx) (str.mname)
#define num_genders 1
#endif

int main(int argc, char* argv[])
{
    FILE* f;
    char name[300];
    const char* command_prefix, *variant_str;

    if (argc <= 1)
    {
        printf("Usage:\n");
        printf("%s [language]\n\n", argv[0]);
        printf("Where language is one of: \n");
        printf("  yaml\n\n");
        return 0;
    }

    variant_str = detect_variant( &command_prefix );
    sprintf( name, "%s.yaml", variant_str );
    if (argc >= 2 && !strcmp(argv[1], "yaml"))
    {
        fprintf( stderr, "Writing to '%s'...\n", variant_str );
        f = fopen(name, "wb");
        if (!f)
        {
            perror("fopen");
            return -1;
        }
        extract_monsterdata_to_yaml(f, variant_str, command_prefix );
        fclose(f);
        fprintf( stderr, "Done.\n" );
    }
    else
    {
        fprintf(stderr,
               "I don't what %s is. Try running %s without "
               "parameters to see the valid languages.\n",
               argv[1], argv[0]);
        return -1;
    }

    return 0;
}

static const char* detect_variant( const char** command_prefix )
{
#ifdef VERSION_ID
    const char* vid = VERSION_ID;
#else
    const char* vid = 0;
#endif
    if ( vid && strstri( vid, "UnNetHackPlus" ) ) {
        (*command_prefix) = "u+";
        return "UnNetHackPlus";
    }
    if ( vid && strstri( vid, "UnNetHack" ) ) {
        (*command_prefix) = "u";
        return "UnNetHack";
    }
    if ( vid && strstri( vid, "SporkHack" ) ) {
        (*command_prefix) = "s";
        return "SporkHack";
    }
    if ( vid && strstri( vid, "NetHack Version 3.4.3" ) ) {
        (*command_prefix) = "v";
        return "Vanilla";
    }

    fprintf( stderr
           , "I don't know what variant this is. "
           "Replace module, data type name and command prefix manually. "
           "Search for 'ReplaceThisName' in the resulting source file.\n" );

    (*command_prefix) = "ReplaceThisName";

    return "ReplaceThisName";
}

#ifdef HAS_MONSTR
extern const int monstr[];
#endif

static void extract_monsterdata_to_yaml(
        FILE* f
      , const char* variant
      , const char* command_prefix )
{
    int i1, i2;
    struct permonst* pm;
    struct monst dummymonst;

    memset(&dummymonst, 0, sizeof(dummymonst));

    /* We want to import everything in this module. */
    fprintf(f, "variant: \"%s\"\n", variant );
    fprintf(f, "prefix: \"%s\"\n\n", command_prefix );

    fprintf(f, "monsters:\n");

    // NetHack 3.6+
    // Comment out if the function does not exist.
#ifdef HAS_MONST_GLOBALS_INIT
    monst_globals_init();
#endif
#ifdef HAS_DNETHACK_ID_PERMONST
    // This fills in 'difficulty' values for all permonst structures
    id_permonst();
#endif

    int num_monsters = -1;
    for (i1 = 0; is_valid_monster(mons[i1]); ++i1) {
        num_monsters = i1;
    }
    num_monsters++;

    for (i1 = 0; is_valid_monster(mons[i1]); ++i1)
    {
        pm = &mons[i1];
        dummymonst.data = pm;
#ifdef HAS_MNUM
        dummymonst.mnum = i1;
#endif

        for (int gender = 0; gender < num_genders; ++gender) {
            if (get_gender_name(mons[i1], gender) == 0) { continue; };
            if (get_gender_name(mons[i1], gender)[0] == 0) { continue; };

        if (i1 > 0 || gender > 0) fprintf(f, "\n");

        fprintf(f, " - name: \"%s\"\n", get_gender_name(mons[i1], gender));
        // NetHack 3.6+ changed how symbols work
        char sym = ' ';
#ifdef MONSYMS_IS_STRUCT
        sym = def_monsyms[pm->mlet].sym;
#else
        sym = def_monsyms[(int) pm->mlet];
#endif
        if (sym == '\"') {
            fprintf(f, "   symbol: \"\\\"\"\n");
        } else {
            fprintf(f, "   symbol: \"%c\"\n", sym);
        }
        fprintf(f, "   base-level: %d\n", pm->mlevel);
#ifdef HAS_MONSTR
        fprintf(f, "   difficulty: %d\n", monstr[(monsndx(pm))]);
#else
        fprintf(f, "   difficulty: %d\n", pm->difficulty);
#endif
        fprintf(f, "   speed: %d\n", pm->mmove);
#ifndef DNETHACK_ACS
        fprintf(f, "   ac: %d\n", pm->ac);
#else
        char acs_str[100];
        sprintf(acs_str, "Nat %d/Dodge %d/Prot %d", pm->nac, pm->dac, pm->pac);
        fprintf(f, "   ac: \"%s\"\n", acs_str);
#endif
        fprintf(f, "   mr: %d\n", pm->mr);
        fprintf(f, "   alignment: %d\n", pm->maligntyp);
        fprintf(f, "   generates:\n");
        if (pm->geno & G_UNIQ)
            fprintf(f, "    - unique\n");
        else if (pm->geno & G_NOGEN)
            fprintf(f, "");
        else
        {
#ifdef HASSHEOL
            if ( !pb_prohibited_by_generation_flags( pm, 1, 1 ) )
                fprintf(f, "    - sheol\n");
            if ( !pb_prohibited_by_generation_flags( pm, 1, 0 ) )
                fprintf(f, "    - gehennom\n");
            if ( !pb_prohibited_by_generation_flags( pm, 0, 0 ) )
                fprintf(f, "    - dungeons\n");
#else
#ifdef G_HELL
#ifdef G_NOHELL
            if ( (pm->geno & G_HELL ||
                  !(pm->geno & G_NOHELL)) ) fprintf(f, "    - gehennom\n");
            if ( !(pm->geno & G_HELL) )
                 fprintf(f, "    - dungeons\n");
#endif
#endif
#endif
        }
        fprintf(f, "   leaves-corpse: %s\n",
                (pm->geno & G_NOCORPSE) ? "No" : "Yes");
        fprintf(f, "   not-generated-normally: %s\n",
                (pm->geno & G_NOGEN) ? "Yes" : "No");
        fprintf(f, "   appears-in-small-groups: %s\n",
                (pm->geno & G_SGROUP) ? "Yes" : "No");
        fprintf(f, "   appears-in-large-groups: %s\n",
                (pm->geno & G_LGROUP) ? "Yes" : "No");
        fprintf(f, "   genocidable: %s\n",
                (pm->geno & G_GENO) ? "Yes" : "No");
        fprintf(f, "   attacks: [");
        for (i2 = 0; i2 < NATTK && (pm->mattk[i2].aatyp ||
                                    pm->mattk[i2].adtyp ||
                                    pm->mattk[i2].damn ||
                                    pm->mattk[i2].damd); ++i2)
        {
            if (i2 > 0)
                fprintf(f, ", ");

            fprintf(f, "[");
#define AT(a, b) else if (pm->mattk[i2].aatyp == a) fprintf(f, "%s", b);
            if (1 + 1 == 3) { } /* I hope we won't run this code
                                   in a universe where 1+1 is 3. */
            AT(AT_NONE, "AtNone")
            AT(AT_CLAW, "AtClaw")
            AT(AT_BITE, "AtBite")
            AT(AT_KICK, "AtKick")
            AT(AT_BUTT, "AtButt")
            AT(AT_TUCH, "AtTouch")
            AT(AT_STNG, "AtSting")
            AT(AT_HUGS, "AtHug")
            AT(AT_SPIT, "AtSpit")
            AT(AT_ENGL, "AtEngulf")
            AT(AT_BREA, "AtBreath")
            AT(AT_EXPL, "AtExplode")
            AT(AT_BOOM, "AtSuicideExplode")
            AT(AT_GAZE, "AtGaze")
            AT(AT_TENT, "AtTentacle")
            AT(AT_WEAP, "AtWeapon")
            AT(AT_MAGC, "AtCast")
#ifdef AT_WBIT
            AT(AT_WBIT, "AtWolfHeadBite")
#endif
#ifdef AT_BRSH
            AT(AT_BRSH, "AtCloseRangeBreath")
#endif
#ifdef AT_XWEP
            AT(AT_XWEP, "AtOffhandedWeapon")
#endif
#ifdef AT_MARI
            AT(AT_MARI, "AtOffOffhandedWeapon")
#endif
#ifdef AT_SCRA
            AT(AT_SCRA, "AtScratch")
#endif
#ifdef AT_LASH
            AT(AT_LASH, "AtLash")
#endif
#ifdef AT_TRAM
            AT(AT_TRAM, "AtTrample")
#endif
#ifdef AT_SRPR
            AT(AT_SRPR, "AtNonContactAttack")
#endif
/* dnethack */
#ifdef AT_TAIL
            AT(AT_TAIL, "AtTailSlap")
#endif
#ifdef AT_OBIT
            AT(AT_OBIT, "AtBite")
#endif
#ifdef AT_ARRW
            AT(AT_ARRW, "AtArrow")
#endif
#ifdef AT_WHIP
            AT(AT_WHIP, "AtWhip")
#endif
#ifdef AT_LRCH
            AT(AT_LRCH, "AtReach")
#endif
#ifdef AT_HODS
            AT(AT_HODS, "AtMirror")
#endif
#ifdef AT_LNCK
            AT(AT_LNCK, "AtReachingBite")
#endif
#ifdef AT_MMGC
            AT(AT_MMGC, "AtMMagical")
#endif
#ifdef AT_ILUR
            AT(AT_ILUR, "AtIllurien")
#endif
#ifdef AT_HITS
            AT(AT_HITS, "AtAutoHit")
#endif
#ifdef AT_WISP
            AT(AT_WISP, "AtWispMist")
#endif
#ifdef AT_TNKR
            AT(AT_TNKR, "AtTinker")
#endif
#ifdef AT_5SQR
            AT(AT_5SQR, "AtReachTouch")
#endif
#ifdef AT_5SBT
            AT(AT_5SBT, "AtReachBite")
#endif
#ifdef AT_WDGZ
            AT(AT_WDGZ, "AtPassiveWideGaze")
#endif
#ifdef AT_REND
            AT(AT_REND, "AtHitsIfTwoPreviousHitsConnect")
#endif
#ifdef AT_VINE
            AT(AT_VINE, "AtLashingVine")
#endif
#ifdef AT_BKGT
            AT(AT_BKGT, "AtBlackGoat")
#endif
#ifdef AT_BKG2
            AT(AT_BKG2, "AtBlackGoat")
#endif
#ifdef AT_XSPR
            AT(AT_XSPR, "AtNonContactAttack")
#endif
#ifdef AT_MSPR
            AT(AT_MSPR, "AtNonContactAttack")
#endif
#ifdef AT_DSPR
            AT(AT_DSPR, "AtNonContactAttack")
#endif
#ifdef AT_ESPR
            AT(AT_ESPR, "AtNonContactAttack")
#endif
#ifdef AT_SHDW
            AT(AT_SHDW, "AtPhaseNonContact")
#endif
#ifdef AT_BEAM
            AT(AT_BEAM, "AtBeamNonContact")
#endif
#ifdef AT_DEVA
            AT(AT_DEVA, "AtMillionArms")
#endif
#ifdef AT_MULTIPLY
            AT(AT_MULTIPLY, "AtMultiply")
#endif
#ifdef AT_NTCH
            AT(AT_NTCH, "AtAdjacent")
#endif
#ifdef AT_TALK
            AT(AT_TALK, "AtTalk")
#endif
#ifdef AT_SCRE
            AT(AT_SCRE, "AtScre")
#endif
#ifdef AT_VOLY
            AT(AT_VOLY, "AtVolley")
#endif
#ifdef AT_ANY
            AT(AT_ANY, "AtAny")
#endif
#ifdef AT_RATH
            AT(AT_RATH, "AtRangedThorns")
#endif
            else { fprintf(stderr,
                    "I don't know what attack type %d is.\n", pm->mattk[i2].aatyp);
                   abort(); }
#undef AT
#define AT(a, b) else if (pm->mattk[i2].adtyp == a) fprintf(f, ", %s", b);
            if (1 == 2) { }
            AT(AD_PHYS, "AdPhys")
            AT(AD_MAGM, "AdMagicMissile")
            AT(AD_FIRE, "AdFire")
            AT(AD_COLD, "AdCold")
            AT(AD_SLEE, "AdSleep")
            AT(AD_DISN, "AdDisintegrate")
            AT(AD_ELEC, "AdElectricity")
            AT(AD_DRST, "AdStrDrain")
            AT(AD_ACID, "AdAcid")
            AT(AD_BLND, "AdBlind")
            AT(AD_STUN, "AdStun")
            AT(AD_SLOW, "AdSlow")
            AT(AD_PLYS, "AdParalyse")
            AT(AD_DRLI, "AdLevelDrain")
            AT(AD_DREN, "AdMagicDrain")
            AT(AD_LEGS, "AdLegs")
            AT(AD_STON, "AdStone")
            AT(AD_STCK, "AdSticking")
            AT(AD_SGLD, "AdGoldSteal")
            AT(AD_SITM, "AdItemSteal")
            AT(AD_SEDU, "AdSeduce")
            AT(AD_TLPT, "AdTeleport")
            AT(AD_RUST, "AdRust")
            AT(AD_CONF, "AdConfuse")
            AT(AD_DGST, "AdDigest")
            AT(AD_HEAL, "AdHeal")
            AT(AD_WRAP, "AdWrap")
            AT(AD_WERE, "AdWere")
            AT(AD_DRDX, "AdDexDrain")
            AT(AD_DRCO, "AdConDrain")
            AT(AD_DRIN, "AdIntDrain")
            AT(AD_DISE, "AdDisease")
            AT(AD_DCAY, "AdRot")
            AT(AD_SSEX, "AdSex")
            AT(AD_HALU, "AdHallucination")
            AT(AD_DETH, "AdDeath")
            AT(AD_PEST, "AdPestilence")
            AT(AD_FAMN, "AdFamine")
            AT(AD_SLIM, "AdSlime")
            AT(AD_ENCH, "AdDisenchant")
            AT(AD_CORR, "AdCorrode")
            AT(AD_CLRC, "AdClerical")
            AT(AD_SPEL, "AdSpell")
            AT(AD_RBRE, "AdRandomBreath")
            AT(AD_SAMU, "AdAmuletSteal")
            AT(AD_CURS, "AdCurse")
#ifdef AD_LARV
            AT(AD_LARV, "AdInjectLarva")
#endif
#ifdef AD_SKEL
            AT(AD_SKEL, "AdMakeSkeletons")
#endif
#ifdef AD_POTN
            AT(AD_POTN, "AdPotionEffect")
#endif
#ifdef AD_KDNP
            AT(AD_KDNP, "AdKidnap")
#endif
#ifdef AD_LAWS
            AT(AD_LAWS, "AdLaws")
#endif
#ifdef AD_LOST
            AT(AD_LOST, "AdGetLost")
#endif
#ifdef AD_MTRL
            AT(AD_MTRL, "AdTransmute")
#endif
#ifdef AD_HYDR
            AT(AD_HYDR, "AdGrowHeads")
#endif
#ifdef AD_MEMR
            AT(AD_MEMR, "AdForgetItems")
#endif
#ifdef AD_WIND
            AT(AD_WIND, "AdWind")
#endif
#ifdef AD_QUIL
            AT(AD_QUIL, "AdQuills")
#endif
#ifdef AD_VOID
            AT(AD_VOID, "AdVoidDisintegrate")
#endif
#ifdef AD_DSRM
            AT(AD_DSRM, "AdDisarm")
#endif
#ifdef AD_LASR
            AT(AD_LASR, "AdLaser")
#endif
#ifdef AD_NUKE
            AT(AD_NUKE, "AdNuke")
#endif
#ifdef AD_UNHY
            AT(AD_UNHY, "AdUnholy")
#endif
#ifdef AD_ROCK
            AT(AD_ROCK, "AdRock")
#endif
#ifdef AD_HOLY
            AT(AD_HOLY, "AdHoly")
#endif
#ifdef AD_LOKO
            AT(AD_LOKO, "AdLokoban")
#endif
#ifdef AD_RNBW
            AT(AD_RNBW, "AdHalluSick")
#endif
#ifdef AD_JACK
            AT(AD_JACK, "AdBigExplosion")
#endif
#ifdef AD_YANK
            AT(AD_YANK, "AdYank")
#endif
#ifdef AD_PAIM
            AT(AD_PAIM, "AdExplodingMMSpellbook")
#endif
#ifdef AD_ALIG
            AT(AD_ALIG, "AdAlignmentBlast")
#endif
#ifdef AD_SPIR
            AT(AD_SPIR, "AdReleaseAlignmentSpirits")
#endif
#ifdef AD_COSM
            AT(AD_COSM, "AdCrystalMemories")
#endif
#ifdef AD_CRYS
            AT(AD_CRYS, "AdDilithiumCrystals")
#endif
#ifdef AD_NUDZ
            AT(AD_NUDZ, "AdMirrorBlast")
#endif
#ifdef AD_WHIS
            AT(AD_WHIS, "AdVoidWhispers")
#endif
#ifdef AD_VAMP
            AT(AD_VAMP, "AdVampireDrain")
#endif
#ifdef AD_DEAD
            AT(AD_DEAD, "AdDeadGaze")
#endif
#ifdef AD_SUCK
            AT(AD_SUCK, "AdSuckEquipment")
#endif
#ifdef AD_CHRN
            AT(AD_CHRN, "AdCursedUnihorn")
#endif
#ifdef AD_RGAZ
            AT(AD_RGAZ, "AdRandomGaze")
#endif
#ifdef AD_LITE
            AT(AD_LITE, "AdLightRay")
#endif
#ifdef AD_NGRA
            AT(AD_NGRA, "AdRemoveEngravings")
#endif
#ifdef AD_GLIB
            AT(AD_GLIB, "AdDisarm")
#endif
#ifdef AD_DARK
            AT(AD_DARK, "AdRemoveLight")
#endif
#ifdef AD_LRVA
            AT(AD_LRVA, "AdImplantEgg")
#endif
#ifdef AD_NPDS
            AT(AD_NPDS, "AdDrainStrength")
#endif
#ifdef AD_NPDD
            AT(AD_NPDD, "AdDrainDexterity")
#endif
#ifdef AD_NPDR
            AT(AD_NPDR, "AdDrainCharisma")
#endif
#ifdef AD_NPDA
            AT(AD_NPDA, "AdDrainsAllSortsOfStuff")
#endif
#ifdef AD_HOOK
            AT(AD_HOOK, "AdFleshHook")
#endif
#ifdef AD_MDWP
            AT(AD_MDWP, "AdMindWipe")
#endif
#ifdef AD_ENDS
            AT(AD_ENDS, "AdPlaceholder")
#endif
#ifdef AD_CHRM
            AT(AD_CHRM, "AdCharm")
#endif
#ifdef AD_SCLD
            AT(AD_SCLD, "AdScald")
#endif
#ifdef AD_EGLD
            AT(AD_EGLD, "AdEatGold")
#endif
#ifdef AD_FLVR
            AT(AD_FLVR, "AdQuarkFlavour")
#endif
#ifdef AD_HNGY
            AT(AD_HNGY, "AdMildHunger")
#endif
#ifdef AD_SHOE
            AT(AD_SHOE, "AdShoe")
#endif
#ifdef AD_WTHR
            AT(AD_WTHR, "AdWither")
#endif
#ifdef AD_DRCO
            AT(AD_DRCO, "AdDrainConstitution")
#endif
#ifdef AD_PITS
            AT(AD_PITS, "AdPitAttack")
#endif
#ifdef AD_SHRD
            AT(AD_SHRD, "AdShred")
#endif
#ifdef AD_CHKH
            AT(AD_CHKH, "AdEscalatingDamage")
#endif
#ifdef AD_NEXU
            AT(AD_NEXU, "AdNexus")
#endif
#ifdef AD_INER
            AT(AD_INER, "AdInertia")
#endif
#ifdef AD_TIME
            AT(AD_TIME, "AdDrainLifeOrStats")
#endif
#ifdef AD_PLAS
            AT(AD_PLAS, "AdPlasma")
#endif
#ifdef AD_GRAV
            AT(AD_GRAV, "AdGravity")
#endif
#ifdef AD_ABDC
            AT(AD_ABDC, "AdAbduction")
#endif
#ifdef AD_UVUU
            AT(AD_UVUU, "AdHeadSpike")
#endif
#ifdef AD_HODS
            AT(AD_HODS, "AdMirror")
#endif
#ifdef AD_AXUS
            AT(AD_AXUS, "AdMultiElementCounterAttackThatAngersTons")
#endif
#ifdef AD_DFOO
            AT(AD_DFOO, "AdDrainsAllSortsOfStuff")
#endif
#ifdef AD_WET
            AT(AD_WET, "AdWet")
#endif
#ifdef AD_FAKE
            AT(AD_FAKE, "AdFakeMessages")
#endif
#ifdef AD_THIR
            AT(AD_THIR, "AdThirsty")
#endif
#ifdef AD_WEEP
            AT(AD_WEEP, "AdWeeping")
#endif
#ifdef AD_WEBS
            AT(AD_WEBS, "AdWebs")
#endif
#ifdef AD_SOUN
            AT(AD_SOUN, "AdSound")
#endif
#ifdef AD_LETHE
            AT(AD_LETHE, "AdLethe")
#endif
#ifdef AD_LETH
            AT(AD_LETH, "AdLethe")
#endif
#ifdef AD_WISD
            AT(AD_WISD, "AdWisdom")
#endif
#ifdef AD_MALK
            AT(AD_MALK, "AdMalk")
#endif
#ifdef AD_BANI
            AT(AD_BANI, "AdBanishment")
#endif
#ifdef AD_WRAT
            AT(AD_WRAT, "AdWrath")
#endif
#ifdef AD_NPRO
            AT(AD_NPRO, "AdNegativeProtection")
#endif
#ifdef AD_DEPR
            AT(AD_DEPR, "AdDepression")
#endif
#ifdef AD_LAZY
            AT(AD_LAZY, "AdLazyness")
#endif
#ifdef AD_MANA
            AT(AD_MANA, "AdMana")
#endif
#ifdef AD_DRCH
            AT(AD_DRCH, "AdCharisma")
#endif
#ifdef AD_DEBU
            AT(AD_DEBU, "AdDebuff")
#endif
#ifdef AD_NIVE
            AT(AD_NIVE, "AdNivellation")
#endif
#ifdef AD_TDRA
            AT(AD_TDRA, "AdTechDrain")
#endif
#ifdef AD_BLAS
            AT(AD_BLAS, "AdBlasphemy")
#endif
#ifdef AD_SSTN
            AT(AD_SSTN, "AdSlowStoning")
#endif
#ifdef AD_DOBT
            AT(AD_DOBT, "AdInflictDoubt")
#endif
#ifdef AD_APCS
            AT(AD_APCS, "AdRevelatoryWhisper")
#endif
#ifdef AD_PULL
            AT(AD_PULL, "AdPull")
#endif
#ifdef AD_MERC
            AT(AD_MERC, "AdMercuryBlade")
#endif
#ifdef AD_BDFN
            AT(AD_BDFN, "AdBloodFrenzy")
#endif
#ifdef AD_SESN
            AT(AD_SESN, "AdFourSeasons")
#endif
#ifdef AD_DROP
            AT(AD_DROP, "AdDropItems")
#endif
#ifdef AD_UNPR
            AT(AD_UNPR, "AdRemoveErosionProof")
#endif
#ifdef AD_FLAM
            AT(AD_FLAM, "AdFlame")
#endif
#ifdef AD_SPHR
            AT(AD_SPHR, "AdCreateSphere")
#endif
#ifdef AD_VORP
            AT(AD_VORP, "AdVorpal")
#endif
#ifdef AD_PERH
            AT(AD_PERH, "AdPerHitDie")
#endif
#ifdef AD_SVPN
            AT(AD_SVPN, "AdSeverePoison")
#endif
#ifdef AD_HLUH
            AT(AD_HLUH, "AdHolyUnholyEnergy")
#endif
#ifdef AD_TSMI
            AT(AD_TSMI, "AdTentaclesStealMagicItems")
#endif
#ifdef AD_BYAK
            AT(AD_BYAK, "AdByakheeEggs")
#endif
#ifdef AD_BSTR
            AT(AD_BSTR, "AdBlackStarRapier")
#endif
/* dnethack, bundled together */
#ifdef AD_UNKNWN
            AT(AD_BIST, "AdBisectBeak")
            AT(AD_MIST, "AdMist")
            AT(AD_SUCK, "AdSuck")
            AT(AD_GROW, "AdGrow")
            AT(AD_SOUL, "AdSoul")
            AT(AD_TELE, "AdTele")
            AT(AD_CHRN, "AdHorn")
            AT(AD_SOLR, "AdSolar")
            AT(AD_SLVR, "AdSilver")
            AT(AD_BALL, "AdIronBall")
            AT(AD_RETR, "AdElementalGaze")
            AT(AD_TENT, "AdTentacle")
            AT(AD_UNKNWN, "AdUnknownPriest")
            AT(AD_POSN, "AdPoison")
            AT(AD_SPNL, "AdLeviathan")
            AT(AD_HLBD, "AdAsmodeusBlood")
            AT(AD_SQUE, "AdStealQuestArtifact")
            AT(AD_KAOS, "AdSpawnChaos")
            AT(AD_WISD, "AdWisdom")
            AT(AD_BLDR, "AdBoulderArrow")
            AT(AD_VBLD, "AdBoulderArrowRandomSpread")
            AT(AD_JAILER, "AdJailer")
            AT(AD_VAMP, "AdVamp")
            AT(AD_BARB, "AdPhysRetaliate")
            AT(AD_GARO, "AdGaro")
            AT(AD_GARO_MASTER, "AdGaroMaster")
            AT(AD_SSUN, "AdSunflower")
            AT(AD_FNEX, "AdFernExplosion")
            AT(AD_MAND, "AdMandrake")
            AT(AD_LOAD, "AdLoadstones")
            AT(AD_ILUR, "AdIllurien")
            AT(AD_TNKR, "AdTinker")
            AT(AD_FRWK, "AdFireworks")
            AT(AD_STDY, "AdStudy")
            AT(AD_DUNSTAN, "AdDunstan")
            AT(AD_IRIS, "AdIris")
            AT(AD_NABERIUS, "AdNaberius")
            AT(AD_OTIAX, "AdOtiax")
            AT(AD_SIMURGH, "AdSimurgh")
            AT(AD_CMSL, "AdColdMissile")
            AT(AD_FMSL, "AdFireMissile")
            AT(AD_EMSL, "AdElectricMissile")
            AT(AD_SMSL, "AdPhysicalShrapnel")
            AT(AD_WMTG, "AdWarMachineGaze")
#endif
#ifdef AD_SPOR
            AT(AD_SPOR, "AdSpore")
#endif
#ifdef AD_LAVA
            AT(AD_LAVA, "AdLava")
#endif
#ifdef AD_CALM
            AT(AD_CALM, "AdCalm")
#endif
#ifdef AD_PIER
            AT(AD_PIER, "AdPiercingScream")
#endif
#ifdef AD_SONG
            AT(AD_SONG, "AdSong")
#endif
#ifdef AD_GIBB
            AT(AD_GIBB, "AdGibber")
#endif
#ifdef AD_TCKL
            AT(AD_TCKL, "AdTickle")
#endif
#ifdef AD_POLY
            AT(AD_POLY, "AdPoly")
#endif
#ifdef AD_PSON
            AT(AD_PSON, "AdPsionic")
#endif
#ifdef AD_BHED
            AT(AD_BHED, "AdBehead")
#endif
#ifdef AD_CNCL
            AT(AD_CNCL, "AdCancellation")
#endif
#ifdef AD_LVLT
            AT(AD_LVLT, "AdLevelTeleport")
            AT(AD_BLNK, "AdBlink")
#endif
#ifdef AD_DRWN
            AT(AD_DRWN, "AdDrown")
#endif
#ifdef G_SHEOL
            AT(AD_FREZ, "AdFreeze")
            AT(AD_PUNI, "AdPunisher")
#endif
#ifdef AD_HEAD
            AT(AD_HEAD, "AdDecapitate")
#endif
#ifdef AD_LUCK
            AT(AD_LUCK, "AdDrainLuck")
#endif
#ifdef AD_PSYC
            AT(AD_PSYC, "AdPsionic")
#endif
#ifdef AD_LOUD
            AT(AD_LOUD, "AdLoud")
#endif
#ifdef AD_CLOB
            AT(AD_CLOB, "AdKnockback")
#endif
#ifdef AD_WATR
            AT(AD_WATR, "AdWater")
#endif
#ifdef AD_OONA
            AT(AD_OONA, "AdOona")
#endif
#ifdef AD_NTZC
            AT(AD_NTZC, "AdNetzach")
#endif
#ifdef AD_WTCH
            AT(AD_WTCH, "AdWatcherTentacleGaze")
#endif
#ifdef AD_STTP
            AT(AD_STTP, "AdStealByTeleportation")
#endif
#ifdef AD_HDRG
            AT(AD_HDRG, "AdHalfDragon")
#endif
#ifdef AD_NPDC
            AT(AD_NPDC, "AdDrainConstitution")
#endif
#ifdef AD_EACD
            AT(AD_EACD, "AdElementalAcid")
#endif
#ifdef AD_CNFT
            AT(AD_CNFT, "AdConflictTouch")
#endif
#ifdef AD_ECLD
            AT(AD_ECLD, "AdElementalCold")
#endif
#ifdef AD_EDRC
            AT(AD_EDRC, "AdElementalPoison")
#endif
#ifdef AD_EFIR
            AT(AD_EFIR, "AdElementalFire")
#endif
#ifdef AD_EELC
            AT(AD_EELC, "AdElementalElectric")
#endif
#ifdef AD_ACFR
            AT(AD_ACFR, "AdArchonFire")
#endif
#ifdef AD_DESC
            AT(AD_DESC, "AdDessicate")
#endif
#ifdef AD_SURY
            AT(AD_SURY, "AdArrowOfSlaying")
#endif
#ifdef AD_BLUD
            AT(AD_BLUD, "AdAntiBloodAttack")
#endif
#ifdef AD_STAR
            AT(AD_STAR, "AdSilverStarlightRapier")
#endif
#ifdef AD_PYCL
            AT(AD_PYCL, "AdFirePoisonPhysicalBlindness")
#endif
#ifdef AD_POLN
            AT(AD_POLN, "AdPollen")
#endif
#ifdef AD_GOLD
            AT(AD_GOLD, "AdGoldify")
#endif
#ifdef AD_MOON
            AT(AD_MOON, "AdMoonlightRapier")
#endif
#ifdef AD_SHDW
            AT(AD_SHDW, "AdBlackWebShadow")
#endif
#ifdef AD_SPORE
            AT(AD_SPORE, "AdSpore")
#endif
#ifdef AD_NUMB
            AT(AD_NUMB, "AdNumb")
#endif
#ifdef AD_FRZE
            AT(AD_FRZE, "AdFreezeSolid")
#endif
#ifdef AD_POIS
            AT(AD_POIS, "AdPoisonStat")
#endif
#ifdef AD_DISP
            AT(AD_DISP, "AdDisplacement")
#endif
#ifdef AD_BURN
            AT(AD_BURN, "AdBurn")
#endif
#ifdef AD_FEAR
            AT(AD_FEAR, "AdFear")
#endif
#ifdef AD_NAST
            AT(AD_NAST, "AdNastyTrap")
#endif
#ifdef AD_SKIL
            AT(AD_SKIL, "AdSkillCapReduce")
#endif
#ifdef AD_DREA
            AT(AD_DREA, "AdDreamAttack")
#endif
#ifdef AD_BADE
            AT(AD_BADE, "AdBadRandomEffect")
#endif
#ifdef AD_FUMB
            AT(AD_FUMB, "AdFumble")
#endif
#ifdef AD_DIMN
            AT(AD_DIMN, "AdDimness")
#endif
#ifdef AD_AMNE
            AT(AD_AMNE, "AdMapAmnesia")
#endif
#ifdef AD_WGHT
            AT(AD_WGHT, "AdIncreaseWeight")
#endif
#ifdef AD_VENO
            AT(AD_VENO, "AdVenomous")
#endif
#ifdef AD_VULN
            AT(AD_VULN, "AdVulnerability")
#endif
#ifdef AD_ICUR
            AT(AD_ICUR, "AdCurseItems")
#endif
#ifdef AD_CAST
            AT(AD_CAST, "AdCast")
#endif
#ifdef AD_CHAO
            AT(AD_CHAO, "AdChaos")
#endif
#ifdef AD_VOMT
            AT(AD_VOMT, "AdVomitInducing")
#endif
#ifdef AD_NGEN
            AT(AD_NGEN, "AdNegativeEnchantment")
#endif
#ifdef AD_VAPO
            AT(AD_VAPO, "AdVaporization")
#endif
#ifdef AD_EDGE
            AT(AD_EDGE, "AdStoneEdge")
#endif
#ifdef AD_LITT
            AT(AD_LITT, "AdLitterBlob")
#endif
#ifdef AD_TRAP
            AT(AD_TRAP, "AdCreateTrap")
#endif
#ifdef AD_RNG
            AT(AD_RNG, "AdRngIntervention")
#endif
#ifdef AD_MIDI
            AT(AD_MIDI, "AdIdentityAttack")
#endif
#ifdef AD_ICEB
            AT(AD_ICEB, "AdIceBlock")
#endif
#ifdef AD_FREN
            AT(AD_FREN, "AdFrenzy")
#endif
#ifdef AD_NTHR
            AT(AD_NTHR, "AdNether")
#endif
#ifdef AD_INSA
            AT(AD_INSA, "AdInsanity")
#endif
#ifdef AD_SLUD
            AT(AD_SLUD, "AdSludge")
#endif
#ifdef AD_MINA
            AT(AD_MINA, "AdIdentityNastiness")
#endif
#ifdef AD_IDAM
            AT(AD_IDAM, "AdItemDamager")
#endif
#ifdef AD_ANTI
            AT(AD_ANTI, "AdAntimatter")
#endif
#ifdef AD_PAIN
            AT(AD_PAIN, "AdPain")
#endif
#ifdef AD_MROT
            AT(AD_MROT, "AdMummyRot")
#endif
#ifdef AD_TECH
            AT(AD_TECH, "AdTech")
#endif
#ifdef AD_MEMO
            AT(AD_MEMO, "AdMemoryReduce")
#endif
#ifdef AD_TRAI
            AT(AD_TRAI, "AdSkillReduce")
#endif
#ifdef AD_STAT
            AT(AD_STAT, "AdStatDamage")
#endif
#ifdef AD_DAMA
            AT(AD_DAMA, "AdGearDamage")
#endif
#ifdef AD_THIE
            AT(AD_THIE, "AdThievery")
#endif
#ifdef AD_RAGN
            AT(AD_RAGN, "AdLavaTiles")
#endif
#ifdef AD_DATA
            AT(AD_DATA, "AdDeletesYourGame")
#endif
#ifdef AD_ALIN
            AT(AD_ALIN, "AdDrainAlignment")
#endif
#ifdef AD_SIN
            AT(AD_SIN, "AdAddSins")
#endif
#ifdef AD_CONT
            AT(AD_CONT, "AdContamination")
#endif
#ifdef AD_AGGR
            AT(AD_AGGR, "AdAggravateMonster")
#endif
#ifdef AD_DEST
            AT(AD_DEST, "AdDestroyEq")
#endif
#ifdef AD_TREM
            AT(AD_TREM, "AdTrembling")
#endif
#ifdef AD_SPC2
            AT(AD_SPC2, "AdMasterBlaster")
#endif
#ifdef AD_ANY
            AT(AD_ANY, "AdAny")
#endif
#ifdef AD_NACU
            AT(AD_NACU, "AdCurseArmor")
#endif
#ifdef AD_SANI
            AT(AD_SANI, "AdIncreaseSanity")
#endif
#ifdef AD_RBAD
            AT(AD_RBAD, "AdReallyBadEffect")
#endif
#ifdef AD_BLEE
            AT(AD_BLEE, "AdBleedout")
#endif
#ifdef AD_SHAN
            AT(AD_SHAN, "AdShank")
#endif
#ifdef AD_SCOR
            AT(AD_SCOR, "AdDrainScore")
#endif
#ifdef AD_TERR
            AT(AD_TERR, "AdTerrainTerror")
#endif
#ifdef AD_FEMI
            AT(AD_FEMI, "AdFeminism")
#endif
#ifdef AD_LEVI
            AT(AD_LEVI, "AdLevitation")
#endif
#ifdef AD_MCRE
            AT(AD_MCRE, "AdReduceMagicCancellation")
#endif
#ifdef AD_ILLU
            AT(AD_ILLU, "AdIllusion")
#endif
#ifdef AD_PART
            AT(AD_PART, "AdSpecificRegularAttack")
#endif
#ifdef AD_RUNS
            AT(AD_RUNS, "AdSpecificNastyTrap")
#endif
            else { fprintf(stderr,
                    "I don't know what attack damage type %d is. (%s) (%d/%d)\n",
                    pm->mattk[i2].adtyp, get_gender_name(mons[i1], gender), i1, num_monsters); abort(); }
#undef AT
            fprintf(f, ", %d, %d]", pm->mattk[i2].damn, pm->mattk[i2].damd);
        }
        fprintf(f, "]\n");
        fprintf(f, "   weight: %d\n", pm->cwt);
        fprintf(f, "   nutrition: %d\n", pm->cnutrit);
        fprintf(f, "   size: ");
        if (pm->msize == MZ_TINY) fprintf(f, "tiny\n");
        else if (pm->msize == MZ_SMALL) fprintf(f, "small\n");
        else if (pm->msize == MZ_MEDIUM) fprintf(f, "medium\n");
        else if (pm->msize == MZ_LARGE) fprintf(f, "large\n");
        else if (pm->msize == MZ_HUGE) fprintf(f, "huge\n");
        else if (pm->msize == MZ_GIGANTIC) fprintf(f, "gigantic\n");
        else { fprintf(stderr,
                "I don't know what size %d means.\n", pm->msize); abort(); }

        fprintf(f, "   resistances:\n");
        if (pm->mresists & MR_FIRE) fprintf(f, "    - ReFire\n");
        if (pm->mresists & MR_COLD) fprintf(f, "    - ReCold\n");
        if (pm->mresists & MR_SLEEP) fprintf(f, "    - ReSleep\n");
        if (pm->mresists & MR_DISINT) fprintf(f, "    - ReDisintegrate\n");
        if (pm->mresists & MR_ELEC) fprintf(f, "    - ReElectricity\n");
        if (pm->mresists & MR_POISON) fprintf(f, "    - RePoison\n");
        if (pm->mresists & MR_ACID) fprintf(f, "    - ReAcid\n");
        if (pm->mresists & MR_STONE) fprintf(f, "    - RePetrification\n");
        if (resists_magm(&dummymonst)) fprintf(f, "    - ReMagic\n");
        if (resists_drli(&dummymonst)) fprintf(f, "    - ReDrain\n");
        fprintf(f, "   conferred:\n");
        if (pm->mconveys & MR_FIRE) fprintf(f, "    - ReFire\n");
        if (pm->mconveys & MR_COLD) fprintf(f, "    - ReCold\n");
        if (pm->mconveys & MR_SLEEP) fprintf(f, "    - ReSleep\n");
        if (pm->mconveys & MR_DISINT) fprintf(f, "    - ReDisintegrate\n");
        if (pm->mconveys & MR_ELEC) fprintf(f, "    - ReElectricity\n");
        if (pm->mconveys & MR_POISON) fprintf(f, "    - RePoison\n");
        if (pm->mconveys & MR_ACID) fprintf(f, "    - ReAcid\n");
        /* You can't actually get petrification resistance. */
        // if (pm->mconveys & MR_STONE) fprintf(f, "    - RePetrification\n");

        fprintf(f, "   flags: [");
        {
            int comma_set = 0;
#ifndef DNETHACK_MONFLAGS
#define AT(a, b) if (pm->mflags1 & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef M1_FLY
        AT(M1_FLY, "FlFly");
#endif
#ifdef M1_NOTAKE
        AT(M1_NOTAKE, "FlNoTake");
#endif
        AT(M1_SWIM, "FlSwim");
        AT(M1_AMORPHOUS, "FlAmorphous");
        AT(M1_WALLWALK, "FlWallwalk");
        AT(M1_CLING, "FlCling");
        AT(M1_TUNNEL, "FlTunnel");
#ifdef M1_NEEDPICK
        AT(M1_NEEDPICK, "FlNeedPick");
#endif
#ifdef M1_CONCEAL
        AT(M1_CONCEAL, "FlConceal");
#endif
#ifdef M1_HIDE
        AT(M1_HIDE, "FlHide");
#endif
#ifdef M1_AMPHIBIOUS
        AT(M1_AMPHIBIOUS, "FlAmphibious");
#endif
#ifdef M1_BREATHLESS
        AT(M1_BREATHLESS, "FlBreathless");
#endif
#ifdef M1_NOEYES
        AT(M1_NOEYES, "FlNoEyes");
#endif
#ifdef M1_NOHANDS
        AT(M1_NOHANDS, "FlNoHands");
#endif
#ifdef M1_NOLIMBS
        AT(M1_NOLIMBS, "FlNoLimbs");
#endif
#ifdef M1_NOHEAD
        AT(M1_NOHEAD, "FlNoHead");
#endif
        AT(M1_MINDLESS, "FlMindless");
#ifdef M1_HUMANOID
        AT(M1_HUMANOID, "FlHumanoid");
#endif
        AT(M1_ANIMAL, "FlAnimal");
#ifdef M1_TUNNEL
        AT(M1_TUNNEL, "FlTunnel");
#endif
#ifdef M1_SLITHY
        AT(M1_SLITHY, "FlSlithy");
#endif
        AT(M1_UNSOLID, "FlUnSolid");
        AT(M1_THICK_HIDE, "FlThickHide");
#ifdef M1_OVIPAROUS
        AT(M1_OVIPAROUS, "FlOviparous");
#endif
        AT(M1_REGEN, "FlRegen");
        AT(M1_SEE_INVIS, "FlSeeInvis");
        AT(M1_TPORT, "FlTeleport");
        AT(M1_TPORT_CNTRL, "FlTeleportControl");
        AT(M1_ACID, "FlAcid");
        AT(M1_POIS, "FlPoisonous");
        AT(M1_CARNIVORE, "FlCarnivore");
        AT(M1_HERBIVORE, "FlHerbivore");
        AT(M1_METALLIVORE, "FlMetallivore");
#undef AT

        if (!polyok(pm)) {
            if ( comma_set ) fprintf(f, ", ");
            comma_set = 1;
            fprintf(f, "FlNoPoly");
        }
        if (touch_petrifies(pm)) {
            if ( comma_set ) fprintf(f, ", ");
            comma_set = 1;
            fprintf(f, "FlTouchPetrifies");
        }
        if (pm_invisible(pm)) {
            if ( comma_set ) fprintf(f, ", ");
            comma_set = 1;
            fprintf(f, "FlInvisible");
        }

#define AT(a, b) if (pm->mflags2 & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef M2_UNDEAD
        AT(M2_UNDEAD, "FlUndead");
#endif
#ifdef M2_WERE
        AT(M2_WERE, "FlWere");
#endif
#ifdef M2_HUMAN
        AT(M2_HUMAN, "FlHuman");
#endif
#ifdef M2_ELF
        AT(M2_ELF, "FlElf");
#endif
#ifdef M2_DWARF
        AT(M2_DWARF, "FlDwarf");
#endif
#ifdef M2_GNOME
        AT(M2_GNOME, "FlGnome");
#endif
#ifdef M2_ORC
        AT(M2_ORC, "FlOrc");
#endif
#ifdef M2_DEMON
        AT(M2_DEMON, "FlDemon");
#endif
#ifdef M2_MERC
        AT(M2_MERC, "FlMerc");
#endif
#ifdef M2_LORD
        AT(M2_LORD, "FlLord");
#endif
#ifdef M2_PRINCE
        AT(M2_PRINCE, "FlPrince");
#endif
#ifdef M2_MINION
        AT(M2_MINION, "FlMinion");
#endif
#ifdef M2_GIANT
        AT(M2_GIANT, "FlGiant");
#endif
#ifdef M2_MALE
        AT(M2_MALE, "FlMale");
#endif
#ifdef M2_FEMALE
        AT(M2_FEMALE, "FlFemale");
#endif
#ifdef M2_NEUTER
        AT(M2_NEUTER, "FlNeuter");
#endif
#ifdef M2_PNAME
        AT(M2_PNAME, "FlProperName");
#endif
#ifdef M2_HOSTILE
        AT(M2_HOSTILE, "FlHostile");
#endif
#ifdef M2_PEACEFUL
        AT(M2_PEACEFUL, "FlPeaceful");
#endif
#ifdef M2_DOMESTIC
        AT(M2_DOMESTIC, "FlDomestic");
#endif
#ifdef M2_WANDER
        AT(M2_WANDER, "FlWander");
#endif
#ifdef M2_STALK
        AT(M2_STALK, "FlStalk");
#endif
#ifdef M2_NASTY
        AT(M2_NASTY, "FlNasty");
#endif
#ifdef M2_STRONG
        AT(M2_STRONG, "FlStrong");
#endif
#ifdef M2_ROCKTHROW
        AT(M2_ROCKTHROW, "FlRockThrow");
#endif
#ifdef M2_GREEDY
        AT(M2_GREEDY, "FlGreedy");
#endif
#ifdef M2_JEWELS
        AT(M2_JEWELS, "FlJewels");
#endif
#ifdef M2_COLLECT
        AT(M2_COLLECT, "FlCollect");
#endif
#ifdef M2_MAGIC
        AT(M2_MAGIC, "FlMagicCollect");
#endif
#undef AT
        if (passes_walls(pm)) {
            fprintf(f, ", FlPhasing");
        }
#define AT(a, b) if (pm->mflags3 & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef M3_WANTSAMUL
        AT(M3_WANTSAMUL, "FlWantsAmulet");
#endif
#ifdef M3_WANTSBELL
        AT(M3_WANTSBELL, "FlWantsBell");
#endif
#ifdef M3_WANTSBOOK
        AT(M3_WANTSBOOK, "FlWantsBook");
#endif
#ifdef M3_WANTSCAND
        AT(M3_WANTSCAND, "FlWantsCand");
#endif
#ifdef M3_WANTSARTI
        AT(M3_WANTSARTI, "FlWantsArti");
#endif
#ifdef M3_WANTSALL
        AT(M3_WANTSALL, "FlWantsAll");
#endif
#ifdef M3_WAITFORU
        AT(M3_WAITFORU, "FlWaitsForYou");
#endif
#ifdef M3_CLOSE
        AT(M3_CLOSE, "FlClose");
#endif
#ifdef M3_COVETOUS
        AT(M3_COVETOUS, "FlCovetous");
#endif
#ifdef M3_LITHIVORE
        AT(M3_LITHIVORE, "FlLithivore");
#endif
#ifdef M3_POKEMON
        AT(M3_POKEMON, "FlPokemon");
#endif
#ifdef M3_AVOIDER
        AT(M3_AVOIDER, "FlAvoider");
#endif
#ifdef M3_NOTAME
        AT(M3_NOTAME, "FlUntameable");
#endif
#ifdef M3_TRAITOR
        AT(M3_TRAITOR, "FlTraitor");
#endif
#ifdef HAS_MON_HATES_MATERIAL
        if (mon_hates_material(&dummymonst, SILVER)) fprintf(f, ", FlHatesSilver");
#endif
#ifdef M3_INFRAVISIBLE
        AT(M3_INFRAVISIBLE, "FlInfravisible");
#endif
#ifdef M3_INFRAVISION
        AT(M3_INFRAVISION, "FlInfravision");
#endif
#ifdef HAS_HATES_SILVER
        if (hates_silver(pm)) fprintf(f, ", FlHatesSilver");
#endif
        if (passes_bars(pm)) {
            if ( comma_set ) fprintf(f, ", ");
            comma_set = 1;
            fprintf(f, "FlPassesBars");
        }
        if (vegan(pm)) fprintf(f, ", FlVegan");
        else if (vegetarian(pm)) fprintf(f, ", FlVegetarian");

#undef AT
#endif // #ifdef DNETHACK_MONFLAGS
#ifdef DNETHACK_MONFLAGS
        if (!polyok(pm)) {
            if ( comma_set ) fprintf(f, ", ");
            comma_set = 1;
            fprintf(f, "FlNoPoly");
        }
        if (touch_petrifies(pm)) {
            if ( comma_set ) fprintf(f, ", ");
            comma_set = 1;
            fprintf(f, "FlTouchPetrifies");
        }
        if (pm_invisible(pm)) {
            if ( comma_set ) fprintf(f, ", ");
            comma_set = 1;
            fprintf(f, "FlInvisible");
        }

        // Mobility flags
#define AT(a, b) if (pm->mflagsm & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef MM_FLY
        AT(MM_FLY, "FlFly");
#endif
#ifdef MM_SWIM
        AT(MM_SWIM, "FlSwim");
#endif
#ifdef MM_AMORPHOUS
        AT(MM_AMORPHOUS, "FlAmorphous");
#endif
#ifdef MM_WALLWALK
        AT(MM_WALLWALK, "FlWallwalk");
#endif
#ifdef MM_CLING
        AT(MM_CLING, "FlCling");
#endif
#ifdef MM_TUNNEL
        AT(MM_TUNNEL, "FlTunnel");
#endif
#ifdef MM_NEEDPICK
        AT(MM_NEEDPICK, "FlNeedPick");
#endif
#ifdef MM_AMPHIBIOUS
        AT(MM_AMPHIBIOUS, "FlAmphibious");
#endif
#ifdef MM_BREATHLESS
        AT(MM_BREATHLESS, "FlBreathless");
#endif
#ifdef MM_TPORT
        AT(MM_TPORT, "FlTeleport");
#endif
#ifdef MM_TPORT_CNTRL
        AT(MM_TPORT_CNTRL, "FlTeleportControl");
#endif
#undef AT
#define AT(a, b) if (pm->mflagst & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef MT_HIDE
        AT(MT_HIDE, "FlHide");
#endif
#ifdef MT_CONCEAL
        AT(MT_CONCEAL, "FlConceal");
#endif
#ifdef MT_CARNIVORE
        AT(MT_CARNIVORE, "FlCarnivore");
#endif
#ifdef MT_HERBIVORE
        AT(MT_HERBIVORE, "FlHerbivore");
#endif
#ifdef MT_METALLIVORE
        AT(MT_METALLIVORE, "FlMetallivore");
#endif
#ifdef MT_CONCEAL
        AT(MT_CONCEAL, "FlConceal");
#endif
#ifdef MT_HIDE
        AT(MT_HIDE, "FlHide");
#endif
#ifdef MT_NOTAKE
        AT(MT_NOTAKE, "FlNoTake");
#endif
#ifdef MT_MINDLESS
        AT(MT_MINDLESS, "FlMindless");
#endif
#ifdef MT_HOSTILE
        AT(MT_HOSTILE, "FlHostile");
#endif
#ifdef MT_PEACEFUL
        AT(MT_PEACEFUL, "FlPeaceful");
#endif
#ifdef MT_DOMESTIC
        AT(MT_DOMESTIC, "FlDomestic");
#endif
#ifdef MT_WANDER
        AT(MT_WANDER, "FlWander");
#endif
#ifdef MT_STALK
        AT(MT_STALK, "FlStalk");
#endif
#ifdef MT_ROCKTHROW
        AT(MT_ROCKTHROW, "FlRockThrow");
#endif
#ifdef MT_GREEDY
        AT(MT_GREEDY, "FlGreedy");
#endif
#ifdef MT_JEWELS
        AT(MT_JEWELS, "FlJewels");
#endif
#ifdef MT_COLLECT
        AT(MT_COLLECT, "FlCollect");
#endif
#ifdef MT_MAGIC
        AT(MT_MAGIC, "FlMagicCollect");
#endif
#ifdef MT_WANTSAMUL
        AT(MT_WANTSAMUL, "FlWantsAmulet");
#endif
#ifdef MT_WANTSBELL
        AT(MT_WANTSBELL, "FlWantsBell");
#endif
#ifdef MT_WANTSBOOK
        AT(MT_WANTSBOOK, "FlWantsBook");
#endif
#ifdef MT_WANTSCAND
        AT(MT_WANTSCAND, "FlWantsCand");
#endif
#ifdef MT_WANTSARTI
        AT(MT_WANTSARTI, "FlWantsArti");
#endif
#ifdef MT_WANTSALL
        AT(MT_WANTSALL, "FlWantsAll");
#endif
#ifdef MT_WAITFORU
        AT(MT_WAITFORU, "FlWaitsForYou");
#endif
#ifdef MT_CLOSE
        AT(MT_CLOSE, "FlClose");
#endif
#ifdef MT_COVETOUS
        AT(MT_COVETOUS, "FlCovetous");
#endif
#ifdef MT_TRAITOR
        AT(MT_TRAITOR, "FlTraitor");
#endif
#undef AT
#define AT(a, b) if (pm->mflagsb & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef MB_ACID
        AT(MB_ACID, "FlAcid");
#endif
#ifdef MB_POIS
        AT(MB_POIS, "FlPoisonous");
#endif
#ifdef MB_NOEYES
        AT(MB_NOEYES, "FlNoEyes");
#endif
#ifdef MB_NOHANDS
        AT(MB_NOHANDS, "FlNoHands");
#endif
#ifdef MB_NOLIMBS
        AT(MB_NOLIMBS, "FlNoLimbs");
#endif
#ifdef MB_NOHEAD
        AT(MB_NOHEAD, "FlNoHead");
#endif
#ifdef MB_HUMANOID
        AT(MB_HUMANOID, "FlHumanoid");
#endif
#ifdef MB_ANIMAL
        AT(MB_ANIMAL, "FlAnimal");
#endif
#ifdef MB_SLITHY
        AT(MB_SLITHY, "FlSlithy");
#endif
#ifdef MB_UNSOLID
        AT(MB_UNSOLID, "FlUnSolid");
#endif
#ifdef MB_THICK_HIDE
        AT(MB_THICK_HIDE, "FlThickHide");
#endif
#ifdef MB_OVIPAROUS
        AT(MB_OVIPAROUS, "FlOviparous");
#endif
#ifdef MB_MALE
        AT(MB_MALE, "FlMale");
#endif
#ifdef MB_FEMALE
        AT(MB_FEMALE, "FlFemale");
#endif
#ifdef MB_NEUTER
        AT(MB_NEUTER, "FlNeuter");
#endif
#ifdef MB_STRONG
        AT(MB_STRONG, "FlStrong");
#endif
#undef AT

#define AT(a, b) if (pm->mflagsg & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef MG_MERC
        AT(MG_MERC, "FlMerc");
#endif
#ifdef MG_LORD
        AT(MG_LORD, "FlLord");
#endif
#ifdef MG_PRINCE
        AT(MG_PRINCE, "FlPrince");
#endif
#ifdef MG_PNAME
        AT(MG_PNAME, "FlProperName");
#endif
#ifdef MG_REGEN
        AT(MG_REGEN, "FlRegen");
#endif
#ifdef MG_NASTY
        AT(MG_NASTY, "FlNasty");
#endif
#ifdef MG_NOTAME
        AT(MG_NOTAME, "FlUntameable");
#endif
#ifdef MG_NOPOLY
        AT(MG_NOPOLY, "FlNoPoly");
#endif
#ifdef MG_INFRAVISIBLE
        AT(MG_INFRAVISIBLE, "FlInfravisible");
#endif
#undef AT // mflagsg

#define AT(a, b) if (pm->mflagsa & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef MA_UNDEAD
        AT(MA_UNDEAD, "FlUndead");
#endif
#ifdef MA_WERE
        AT(MA_WERE, "FlWere");
#endif
#ifdef MA_HUMAN
        AT(MA_HUMAN, "FlHuman");
#endif
#ifdef MA_ELF
        AT(MA_ELF, "FlElf");
#endif
#ifdef MA_DWARF
        AT(MA_DWARF, "FlDwarf");
#endif
#ifdef MA_GNOME
        AT(MA_GNOME, "FlGnome");
#endif
#ifdef MA_ORC
        AT(MA_ORC, "FlOrc");
#endif
#ifdef MA_VAMPIRE
        AT(MA_DEMON, "FlVampire");
#endif
#ifdef MA_CLOCK
        AT(MA_CLOCK, "FlClockwork");
#endif
#ifdef MA_DEMON
        AT(MA_DEMON, "FlDemon");
#endif
#ifdef MA_MINION
        AT(MA_MINION, "FlMinion");
#endif
#ifdef MA_GIANT
        AT(MA_GIANT, "FlGiant");
#endif
#undef AT // mflagsa

#define AT(a, b) if (pm->mflagsv & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef MV_INFRAVISION
        AT(MV_INFRAVISION, "FlInfravision");
#endif
#ifdef MV_SEE_INVIS
        AT(MV_SEE_INVIS, "FlSeeInvis");
#endif
#undef AT // mflagsv

        if (species_passes_walls(pm)) {
            fprintf(f, ", FlPhasing");
        }
#ifdef HAS_HATES_SILVER
        if (hates_silver(pm)) fprintf(f, ", FlHatesSilver");
#endif
        if (passes_bars(&dummymonst)) {
            if ( comma_set ) fprintf(f, ", ");
            comma_set = 1;
            fprintf(f, "FlPassesBars");
        }
        if (vegan(pm)) fprintf(f, ", FlVegan");
        else if (vegetarian(pm)) fprintf(f, ", FlVegetarian");
#endif // DNETHACK_MONFLAGS

#define MH(a, b) if (pm->mhflags & (a)) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#ifdef HAS_RACEBOOLEAN_BITFLAGS
        MH(MH_GIANT, "FlGiant")
        MH(MH_UNDEAD, "FlUndead")
        MH(MH_HUMAN, "FlHuman")
        MH(MH_GNOME, "FlGnome")
        MH(MH_ORC, "FlOrc")
        MH(MH_WERE, "FlWere")
        MH(MH_DEMON, "FlDemon")
#endif
        }
        fprintf(f, "]\n");
        fprintf(f, "   color: ");
        switch(pm->mcolor)
        {
            case CLR_BLACK: fprintf(f, "Black"); break;
            case CLR_RED: fprintf(f, "Red"); break;
            case CLR_GREEN: fprintf(f, "Green"); break;
            case CLR_BROWN: fprintf(f, "Brown"); break;
            case CLR_BLUE: fprintf(f, "Blue"); break;
            case CLR_MAGENTA: fprintf(f, "Magenta"); break;
            case CLR_CYAN: fprintf(f, "Cyan"); break;
            case CLR_GRAY: fprintf(f, "Gray"); break;
            case CLR_ORANGE: fprintf(f, "Orange"); break;
            case CLR_BRIGHT_GREEN: fprintf(f, "BrightGreen"); break;
            case CLR_BRIGHT_BLUE: fprintf(f, "BrightBlue"); break;
            case CLR_BRIGHT_CYAN: fprintf(f, "BrightCyan"); break;
            case CLR_BRIGHT_MAGENTA: fprintf(f, "BrightMagenta"); break;
            case CLR_YELLOW: fprintf(f, "Yellow"); break;
            case CLR_WHITE: fprintf(f, "White"); break;
            default: fprintf(stderr, "I don't know what color %d is.\n",
                             pm->mcolor);
                     abort();
        }
        fprintf(f, "\n");
    }
    }
    fprintf(f, "all-monster-names: [");
    int first = 1;
    for (i1 = 0; is_valid_monster(mons[i1]); ++i1)
    {
        for (int gender = 0; gender < num_genders; ++gender) {
            if (get_gender_name(mons[i1], gender) != 0) {
                if (!first)
                    fprintf(f, ", ");
                first = 0;
                fprintf(f, "\"%s\"", get_gender_name(mons[i1], gender));
            }
        }
    }
    fprintf(f, "]\n\n");
}

#ifdef HASSHEOL
static boolean pb_prohibited_by_generation_flags(struct permonst *ptr
        , int inhell, int insheol)
{
	if (inhell && !insheol) {
		/* In Gehennon, outside of the Sheol */
		if (ptr->geno & G_HELL) {
			return FALSE;
		}
		if (ptr->geno & G_NOHELL) {
			return TRUE;
		}
		if (ptr->geno & G_SHEOL) {
			return TRUE;
		}
		return FALSE;
	} else if (insheol) {
		/* In Sheol */
		if (ptr->geno & G_SHEOL) {
			return FALSE;
		}
		if (ptr->geno & G_NOSHEOL) {
			return TRUE;
		}
		if (ptr->geno & G_HELL) {
			return TRUE;
		}
		return FALSE;
	} else {
		/* Outside of Gehennon and Sheol*/
		if (ptr->geno & G_SHEOL) {
			return TRUE;
		}
		if (ptr->geno & G_HELL) {
			return TRUE;
		}
		return FALSE;
	}
}
#endif

#define main not_main
