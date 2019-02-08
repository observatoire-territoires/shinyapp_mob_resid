### Sources statistiques

Deux sources statistiques sont utilis&eacute;es au sein de cette application :

-   Le [fichier
    d&eacute;tail](https://www.insee.fr/fr/statistiques/3566042?sommaire=3558417)
    du recensement de la population (RP) de l&apos;Insee portant sur les
    mobilit&eacute;s r&eacute;sidentielles:

Il permet de conna&icirc;tre, pour les personnes r&eacute;sidant en France, leur lieu
de r&eacute;sidence au 1er janvier de l&apos;ann&eacute;e pr&eacute;c&eacute;dant l&apos;enqu&ecirc;te. Le RP ne
permet pas de mesurer les d&eacute;parts &agrave; l&apos;&eacute;tranger (les individus n&apos;y &eacute;tant
pas recens&eacute;s ; en revanche les arriv&eacute;es en France sont, elles bien
prises en compte), ni de conna&icirc;tre les mobilit&eacute;s des populations
sp&eacute;cifiques suivantes :

-- les enfants n&eacute;s au cours de l&apos;ann&eacute;e pr&eacute;c&eacute;dant l&apos;enqu&ecirc;te (donc &acirc;g&eacute;s de
moins d&apos;un an) ;

-- les d&eacute;tenus, les personnes vivant en habitation mobile, les
sans-abris et les mariniers ;

-- les personnes vivant &agrave; Mayotte (le RP n&apos;y est pas encore aussi
complet que dans les autres Dom).

Le mill&eacute;sime du recensement de l&apos;Insee utilis&eacute; ici est 2015, le dernier
disponible &agrave; ce jour, qui porte sur les enqu&ecirc;tes annuelles de 2013 &agrave;
2017.

-   Les [s&eacute;ries historiques de population depuis
    1968](https://www.insee.fr/fr/statistiques/3565661)

Afin de conna&icirc;tre les dynamiques d&eacute;mographiques des territoires sur le
long terme, il est n&eacute;cessaire de se r&eacute;f&eacute;rer aux donn&eacute;es de recensement
coupl&eacute;es &agrave; celles de l&apos;Etat civil. Ces fichiers pr&eacute;sentent, par commune
et par p&eacute;riode intercensitaire depuis 1968, le nombre d&apos;habitants, de
naissances et de d&eacute;c&egrave;s. Il est ainsi possible d&apos;en d&eacute;duire pour le
territoire souhait&eacute; des indicateurs : 
-- d&apos;&eacute;volution d&eacute;mographique
totale par p&eacute;riode 
-- d&apos;&eacute;volution d&eacute;mographique due au solde naturel
(r&eacute;sultant de la diff&eacute;rence entre naissances et d&eacute;c&egrave;s) 
-- d&apos;&eacute;volution
d&eacute;mographique due au solde migratoire apparent (obtenue par diff&eacute;rence
entre &eacute;volution d&eacute;mographique totale et &eacute;volution due au solde naturel).

Le mill&eacute;sime du recensement de l&apos;Insee utilis&eacute; ici est 2015, le dernier
disponible &agrave; ce jour.

Pour gagner en lisibilit&eacute;, les valeurs absolues sont ici arrondies &agrave; la
dizaine et les pourcentages arrondis &agrave; deux d&eacute;cimales.

Le p&eacute;rim&egrave;tre des intercommunalit&eacute;s correspond &agrave; celui en vigueur au 1er
janvier 2018.

### M&eacute;thodologie <img src="https://raw.githubusercontent.com/observatoire-territoires/migR/master/man/figures/logo_migr.png" align="right" width=150 />

D&eacute;velopp&eacute; avec le logiciel open-source R, l&apos;objectif du [package
migR](https://github.com/observatoire-territoires/migR) est de faciliter
l&apos;exploitation des fichiers du recensement de l&apos;Insee d&eacute;crivant les
mobilit&eacute;s r&eacute;sidentielles. Il propose &eacute;galement des indicateurs
quantifiant les &eacute;changes entre territoires &agrave; plusieurs &eacute;chelles
g&eacute;ographiques, ainsi que des indicateurs caract&eacute;risant l&apos;impact des
mobilit&eacute;s r&eacute;sidentielles dans la composition socio-d&eacute;mographique des
territoires.

Deux articles documentant le package sont en ligne :

-   une [note
    m&eacute;thodologique](https://observatoire-territoires.github.io/migR/articles/methodo_migr.html)
    pr&eacute;sentant les concepts n&eacute;cessaires &agrave; l&apos;&eacute;tude des mobilit&eacute;s
    r&eacute;sidentielles.
-   un
    [tutoriel](https://observatoire-territoires.github.io/migR/articles/tutorial_fonctions_migr.html)
    d&eacute;taillant les fonctions du package.

Le calcul de l&apos;ensemble des indicateurs valoris&eacute;s dans cette application
peut donc &ecirc;tre facilement effectu&eacute; gr&acirc;ce aux fonctions propos&eacute;es dans le
package migR. L&apos;analyse des mobilit&eacute;s r&eacute;sidentielles bas&eacute;es sur des
sources de donn&eacute;es diff&eacute;rentes ou &agrave; d&apos;autres mailles d&apos;analyse est
&eacute;galement possible.

### Application

Le code source de cet outil est disponible sur le [compte github de
l&apos;Observatoire des
territoires](https://github.com/observatoire-territoires/app_shiny_migres)
: depuis cette page il est possible d&apos;y remonter tout probl&egrave;me technique
rencontr&eacute; lors de son utilisation, mais &eacute;galement les &eacute;volutions
souhait&eacute;es.
