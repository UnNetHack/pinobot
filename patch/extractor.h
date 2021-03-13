/*** EXTRACTOR STUFF ***/

// Go to unixmain.c and slap #include "extractor.h" after all the other
// includes. This overwrites the main() function with the extractor stuff.

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

//#define HAS_RACEBOOLEAN_BITFLAGS    // Has 'mhflags' in permonst
#define HAS_MONST_GLOBALS_INIT
#define GENDERED_NAMES   // Has pmnames instead of mname
//#define HAS_MONSTR       // has monstr[idx] instead of mons.difficulty


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

    for (i1 = 0; is_valid_monster(mons[i1]); ++i1)
    {
        pm = &mons[i1];
        dummymonst.data = pm;

        for (int gender = 0; gender < num_genders; ++gender) {
            if (get_gender_name(mons[i1], gender) == 0) { continue; };
            if (get_gender_name(mons[i1], gender)[0] == 0) { continue; };

        if (i1 > 0 || gender > 0) fprintf(f, "\n");

        fprintf(f, " - name: \"%s\"\n", get_gender_name(mons[i1], gender));
        // NetHack 3.6+ changed how symbols work
        fprintf(f, "   symbol: \"%c\"\n", def_monsyms[pm->mlet].sym);
        //fprintf(f, "   symbol: \"%c\"\n", def_monsyms[pm->mlet]);
        fprintf(f, "   base-level: %d\n", pm->mlevel);
#ifdef HAS_MONSTR
        fprintf(f, "   difficulty: %d\n", monstr[(monsndx(pm))]);
#else
        fprintf(f, "   difficulty: %d\n", pm->difficulty);
#endif
        fprintf(f, "   speed: %d\n", pm->mmove);
        fprintf(f, "   ac: %d\n", pm->ac);
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
#ifdef AT_SCRA
            AT(AT_SCRA, "AtScratch")
#endif
#ifdef AT_LASH
            AT(AT_LASH, "AtLash")
#endif
#ifdef AT_TRAM
            AT(AT_TRAM, "AtTrample")
#endif
/* dnethack */
#ifdef AT_ARRW
            AT(AT_ARRW, "AtArrow")
            AT(AT_WHIP, "AtWhip")
            AT(AT_LRCH, "AtReach")
            AT(AT_HODS, "AtMirror")
            AT(AT_LNCK, "AtReachingBite")
            AT(AT_MMGC, "AtMMagical")
            AT(AT_ILUR, "AtIllurien")
            AT(AT_HITS, "AtAutoHit")
            AT(AT_WISP, "AtWispMist")
            AT(AT_TNKR, "AtTinker")
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
#ifdef AT_SCRE
            AT(AT_SCRE, "AtScre")
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
#ifdef AD_ENDS
            AT(AD_ENDS, "AdPlaceholder")
#endif
#ifdef AD_WTHR
            AT(AD_WTHR, "AdWither")
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
#ifdef AD_DROP
            AT(AD_DROP, "AdDropItems")
#endif
#ifdef AD_UNPR
            AT(AD_UNPR, "AdRemoveErosionProof")
#endif
#ifdef AD_FLAM
            AT(AD_FLAM, "AdFlame")
#endif
/* dnethack, bundled together */
#ifdef AD_UNKNWN
            AT(AD_VORP, "AdVorpal")
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
#ifdef AD_TCKL
            AT(AD_TCKL, "AdTickle")
#endif
#ifdef AD_POLY
            AT(AD_POLY, "AdPoly")
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
#ifdef AD_STAR
            AT(AD_STAR, "AdSilverStarlightRapier")
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
                    "I don't know what attack damage type %d is. (%s)\n",
                    pm->mattk[i2].adtyp, get_gender_name(mons[i1], gender)); abort(); }
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
#define AT(a, b) if (pm->mflags1 & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
        AT(M1_FLY, "FlFly");
        AT(M1_SWIM, "FlSwim");
        AT(M1_AMORPHOUS, "FlAmorphous");
        AT(M1_WALLWALK, "FlWallwalk");
        AT(M1_CLING, "FlCling");
        AT(M1_TUNNEL, "FlTunnel");
        AT(M1_NEEDPICK, "FlNeedPick");
        AT(M1_CONCEAL, "FlConceal");
        AT(M1_HIDE, "FlHide");
        AT(M1_AMPHIBIOUS, "FlAmphibious");
        AT(M1_BREATHLESS, "FlBreathless");
        AT(M1_NOTAKE, "FlNoTake");
        AT(M1_NOEYES, "FlNoEyes");
        AT(M1_NOHANDS, "FlNoHands");
        AT(M1_NOLIMBS, "FlNoLimbs");
        AT(M1_NOHEAD, "FlNoHead");
        AT(M1_MINDLESS, "FlMindless");
        AT(M1_HUMANOID, "FlHumanoid");
        AT(M1_ANIMAL, "FlAnimal");
        AT(M1_SLITHY, "FlSlithy");
        AT(M1_UNSOLID, "FlUnSolid");
        AT(M1_THICK_HIDE, "FlThickHide");
        AT(M1_OVIPAROUS, "FlOviparous");
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
#define AT(a, b) if (pm->mflags2 & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
#define MH(a, b) if (pm->mhflags & (a)) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
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
#ifdef HAS_RACEBOOLEAN_BITFLAGS
        MH(MH_GIANT, "FlGiant")
        MH(MH_UNDEAD, "FlUndead")
        MH(MH_HUMAN, "FlHuman")
        MH(MH_GNOME, "FlGnome")
        MH(MH_ORC, "FlOrc")
        MH(MH_WERE, "FlWere")
        MH(MH_DEMON, "FlDemon")
#endif
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
        AT(M2_MERC, "FlMerc");
        AT(M2_LORD, "FlLord");
        AT(M2_PRINCE, "FlPrince");
        AT(M2_MINION, "FlMinion");
#ifdef M2_GIANT
        AT(M2_GIANT, "FlGiant");
#endif
        AT(M2_MALE, "FlMale");
        AT(M2_FEMALE, "FlFemale");
        AT(M2_NEUTER, "FlNeuter");
        AT(M2_PNAME, "FlProperName");
        AT(M2_HOSTILE, "FlHostile");
        AT(M2_PEACEFUL, "FlPeaceful");
        AT(M2_DOMESTIC, "FlDomestic");
        AT(M2_WANDER, "FlWander");
        AT(M2_STALK, "FlStalk");
        AT(M2_NASTY, "FlNasty");
        AT(M2_STRONG, "FlStrong");
        AT(M2_ROCKTHROW, "FlRockThrow");
        AT(M2_GREEDY, "FlGreedy");
        AT(M2_JEWELS, "FlJewels");
        AT(M2_COLLECT, "FlCollect");
        AT(M2_MAGIC, "FlMagicCollect");
#undef AT
        if (passes_walls(pm)) {
            fprintf(f, ", FlPhasing");
        }
#define AT(a, b) if (pm->mflags3 & a) { \
    if ( comma_set ) fprintf(f, ", "); \
    comma_set = 1;\
    fprintf(f, "%s", b); \
}
        AT(M3_WANTSAMUL, "FlWantsAmulet");
        AT(M3_WANTSBELL, "FlWantsBell");
        AT(M3_WANTSBOOK, "FlWantsBook");
        AT(M3_WANTSCAND, "FlWantsCand");
        AT(M3_WANTSARTI, "FlWantsArti");
        AT(M3_WANTSALL, "FlWantsAll");
        AT(M3_WAITFORU, "FlWaitsForYou");
        AT(M3_CLOSE, "FlClose");
        AT(M3_COVETOUS, "FlCovetous");
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
