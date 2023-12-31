;;;======================================================
;;;   Tugas IF4070
;;;   Prediksi Attire Menggunakan CLIPS
;;;======================================================

;;;*****************************
;;;* VALIDATION INPUT FUNCTION *
;;;*****************************

(deffunction ask_question (?question)
   (bind $?allowed (create$ yes no))
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
         then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed)) do
      (printout t ?question)
      (bind ?answer (read)))
      (if (lexemep ?answer) 
         then (bind ?answer (lowcase ?answer)))
   ?answer
)

(deffunction get_value (?question)
   (bind ?response (ask_question ?question))
   ?response)

;;;************************
;;;*  TEMPLATE FOR USER   *
;;;************************

(deftemplate user
    (slot ID         (type INTEGER)  (default ?NONE))
    (slot outdoor    (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot workout    (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot sleep      (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot cooking    (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot party      (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot hot        (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot formal     (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot dresscode  (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot male       (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot cosplay    (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot halloween  (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot rainy      (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot work       (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot snowy      (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot school     (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot beach      (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot sport      (type SYMBOL)   (default NONE) (allowed-symbols NONE yes no))
    (slot result     (type SYMBOL)   (default NONE))
)

(deffacts insert-facts
    (user (ID 0))
)

;;;************************************
;;;*    USER ATTIRE CHECKING RULES    *
;;;************************************

; ROOT
(defrule outdoor_check
    ; (system_banner)
    ?p <- (user (ID ?ID) (outdoor NONE) (result NONE))
    =>
    (modify ?p (outdoor (get_value "Are you at outdoor? ")))
)

; ******* RULE 2 *******
(defrule hot_outdoor_check
    ?p <- (user (outdoor yes) (hot NONE) (result NONE))
    =>
    (modify ?p (hot (get_value "Is it hot outdoor? ")))
)

; ******* RULE 3 *******
(defrule work_check
    ?p <- (user (hot yes) (outdoor yes) (work NONE) (result NONE))
    =>
    (modify ?p (work (get_value "Are you working right now? ")))
)

; ******* RULE 4 *******
(defrule school_check
    ?p <- (user (work no) (school NONE) (result NONE))
    =>
    (modify ?p (school (get_value "Are you at school right now? ")))
)

; ******* RULE 5 *******
(defrule school_formal_check
    ?p <- (user (school yes) (formal NONE) (result NONE))
    =>
    (modify ?p (formal (get_value "Is your school requires you to wear uniform? ")))
)

; ******* RULE 6 *******
(defrule workout_outdoor_check
    ?p <- (user (school no) (outdoor yes) (workout NONE) (result NONE))
    =>
    (modify ?p (workout (get_value "Are you doing workout right now? ")))
)

; ******* RULE 7 *******
(defrule beach_check
    ?p <- (user (workout no) (outdoor yes) (beach NONE) (result NONE))
    =>
    (modify ?p (beach (get_value "Are you at the beach? ")))
)

; ******* RULE 8 *******
(defrule sport_check
    ?p <- (user (beach no) (sport NONE) (result NONE))
    =>
    (modify ?p (school (get_value "Do you do sport right now? ")))
)

; ******* RULE 9 *******
(defrule rainy_check
    ?p <- (user (hot no) (outdoor yes) (rainy NONE) (result NONE))
    =>
    (modify ?p (rainy (get_value "Is it raining right now? ")))
)

; ******* RULE 10 *******
(defrule snowy_check
    ?p <- (user (rainy no) (outdoor yes) (snowy NONE) (result NONE))
    =>
    (modify ?p (snowy (get_value "Is it snowing right now? ")))
)

; ******* RULE 11 *******
(defrule workout_indoor_check
    ?p <- (user (outdoor no) (workout NONE) (result NONE))
    =>
    (modify ?p (workout (get_value "Are you doing workout right now? ")))
)

; ******* RULE 12 *******
(defrule sleep_check
    ?p <- (user (workout no) (outdoor no) (sleep NONE) (result NONE))
    =>
    (modify ?p (sleep (get_value "Are you going to sleep right now? ")))
)

; ******* RULE 13 *******
(defrule cooking_check
    ?p <- (user (sleep no) (cooking NONE) (result NONE))
    =>
    (modify ?p (cooking (get_value "Are you going to cook right now? ")))
)

; ******* RULE 14 *******
(defrule party_check
    ?p <- (user (cooking no) (party NONE) (result NONE))
    =>
    (modify ?p (party (get_value "Are you going to party right now? ")))
)

; ******* RULE 15 *******
(defrule party_formal_check
    ?p <- (user (party yes) (outdoor no) (formal NONE) (result NONE))
    =>
    (modify ?p (formal (get_value "Is it a formal party right now? ")))
)

; ******* RULE 16 *******
(defrule male_check
    ?p <- (user (formal yes) (party yes) (male NONE) (result NONE))
    =>
    (modify ?p (male (get_value "Are you male? ")))
)

; ******* RULE 17 *******
(defrule dresscode_check
    ?p <- (user (formal no) (party yes) (dresscode NONE) (result NONE))
    =>
    (modify ?p (dresscode (get_value "Does the party require a dress code? ")))
)

; ******* RULE 18 *******
(defrule cosplay_check
    ?p <- (user (dresscode yes) (formal no) (cosplay NONE) (result NONE))
    =>
    (modify ?p (cosplay (get_value "Is it a cosplay party? ")))
)

; ******* RULE 19 *******
(defrule halloween_check
    ?p <- (user (cosplay no) (halloween NONE) (result NONE))
    =>
    (modify ?p (halloween (get_value "Is it halloween party? ")))
)

; ******* RULE 20 *******
(defrule hot_indoor_check
    ?p <- (user (party no) (outdoor no) (hot NONE) (result NONE))
    =>
    (modify ?p (hot (get_value "Is it hot right now? ")))
)

;;;********************************
;;;*       INFERENCE RULES        *
;;;********************************

(defrule is_business_attire
    ?p <- (user (outdoor yes) (hot yes) (work yes))
    =>
    (modify ?p (result business_attire))
)

(defrule is_uniform
    ?p <- (user (outdoor yes) (hot yes) (work no) (school yes) (formal yes))
    =>
    (modify ?p (result uniform))
)

(defrule is_casual
    ?p <- (user (outdoor yes) (hot yes) (work no) (school yes) (formal no))
    =>
    (modify ?p (result casual))
)

(defrule is_sportwear_1
    ?p <- (user (outdoor yes) (hot yes) (work no) (school no) (workout yes))
    =>
    (modify ?p (result sportwear))
)

(defrule is_swimsuit
    ?p <- (user (outdoor yes) (hot yes) (work no) (school no) (workout no) (beach yes))
    =>
    (modify ?p (result swimsuit))
)

(defrule is_sportwear_2
    ?p <- (user (outdoor yes) (hot yes) (work no) (school no) (workout no) (beach no) (sport yes))
    =>
    (modify ?p (result sportwear))
)

(defrule is_t_shirt_jeans
    ?p <- (user (outdoor yes) (hot yes) (work no) (school no) (workout no) (beach no) (sport no))
    =>
    (modify ?p (result t_shirt_jeans))
)

(defrule is_raincoat
    ?p <- (user (outdoor yes) (hot no) (rainy yes))
    =>
    (modify ?p (result raincoat))
)

(defrule is_winter_cloth
    ?p <- (user (outdoor yes) (hot no) (rainy no) (snowy yes))
    =>
    (modify ?p (result winter_cloth))
)

(defrule is_sweater
    ?p <- (user (outdoor yes) (hot no) (rainy no) (snowy no))
    =>
    (modify ?p (result sweater))
)

(defrule is_sportwear_3
    ?p <- (user (outdoor no) (workout yes))
    =>
    (modify ?p (result sportwear))
)

(defrule is_pajamas
    ?p <- (user (outdoor no) (workout no) (sleep yes))
    =>
    (modify ?p (result pajamas))
)

(defrule is_apron
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking yes))
    =>
    (modify ?p (result apron))
)

(defrule is_tuxedo
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking no) (party yes) (formal yes) (male yes))
    =>
    (modify ?p (result tuxedo))
)

(defrule is_dress
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking no) (party yes) (formal yes) (male no))
    =>
    (modify ?p (result dress))
)

(defrule is_cosplay_outfit
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking no) (party yes) (formal no) (dresscode yes) (cosplay yes))
    =>
    (modify ?p (result cosplay_outfit))
)

(defrule is_halloween_outfit
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking no) (party yes) (formal no) (dresscode yes) (cosplay no) (halloween yes))
    =>
    (modify ?p (result halloween_outfit))
)

(defrule is_dresscode_outfit
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking no) (party yes) (formal no) (dresscode yes) (cosplay no) (halloween no))
    =>
    (modify ?p (result dresscode_outfit))
)

(defrule is_house_party_outfit
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking no) (party yes) (formal no) (dresscode no))
    =>
    (modify ?p (result house_party_outfit))
)

(defrule is_soft_fabric_attire
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking no) (party no) (hot yes))
    =>
    (modify ?p (result soft_fabric_attire))
)

(defrule is_sweatshirt
    ?p <- (user (outdoor no) (workout no) (sleep no) (cooking no) (party no) (hot no))
    =>
    (modify ?p (result sweatshirt))
)

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system_banner 
    (declare (salience 10))
    =>
    (printout t "=====================================================" crlf)
    (printout t "+         Prediksi Attire Menggunakan CLIPS         +" crlf)
    (printout t "=====================================================" crlf)
)

(defrule print_result
    (user (ID ?id) (result ?r))
    (not (eq ?r NONE))
    =>
    (if (eq ?r business_attire)     then (bind ?p "Business attire"))
    (if (eq ?r uniform)             then (bind ?p "Uniform"))
    (if (eq ?r casual)              then (bind ?p "Casual"))
    (if (eq ?r sportwear)           then (bind ?p "Sportwear"))
    (if (eq ?r swimsuit)            then (bind ?p "Swimsuit"))
    (if (eq ?r t_shirt_jeans)       then (bind ?p "T-shirt and jeans"))
    (if (eq ?r raincoat)            then (bind ?p "Raincoat"))
    (if (eq ?r winter_cloth)        then (bind ?p "Winter cloth"))
    (if (eq ?r sweater)             then (bind ?p "Sweater"))
    (if (eq ?r pajamas)             then (bind ?p "Pajamas"))
    (if (eq ?r apron)               then (bind ?p "Apron"))
    (if (eq ?r tuxedo)              then (bind ?p "Tuxedo"))
    (if (eq ?r dress)               then (bind ?p "Dress"))
    (if (eq ?r cosplay_outfit)      then (bind ?p "Cosplay outfit"))
    (if (eq ?r halloween_outfit)    then (bind ?p "Halloween Outfit"))
    (if (eq ?r dresscode_outfit)    then (bind ?p "Dresscode outfit"))
    (if (eq ?r house_party_outfit)  then (bind ?p "House party outfit"))
    (if (eq ?r soft_fabric_attire)  then (bind ?p "Soft-fabric attire"))
    (if (eq ?r sweatshirt)          then (bind ?p "Sweatshirt"))
    (printout t "You should wear " ?p crlf)
)