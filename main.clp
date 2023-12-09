;;;======================================================
;;;   Tugas IF4070
;;;   Prediksi Attire Menggunakan CLIPS
;;;======================================================

;;;*****************************
;;;* VALIDATION INPUT FUNCTION *
;;;*****************************

(deffunction ask_question (?question)
   (bind $?allowed (create$ true false))
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
         then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed)) do
      (printout t ?question)
      (bind ?answer (read)))
      (if (lexemep ?answer) 
         then (bind ?answer (lowcase ?answer)))
   ?answer)

(deffunction get_value (?question)
   (bind ?response (ask_question ?question))
   ?response)

;;;************************
;;;*  TEMPLATE FOR JEFF   *
;;;************************

(deftemplate jeff
    (slot ID         (type INTEGER)  (default ?NONE))
    (slot outdoor    (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot workout    (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot sleep      (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot cooking    (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot party      (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot hot        (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot formal     (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot dresscode  (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot male       (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot cosplay    (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot halloween  (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot rainy      (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot work       (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot snowy      (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot school     (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot beach      (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot sport      (type SYMBOL)   (default NONE) (allowed-symbols NONE true false))
    (slot result     (type SYMBOL)   (default NONE))
)

(deffacts insert-facts
    (jeff (ID 0))
)

;;;************************************
;;;*    JEFF ATTIRE CHECKING RULES    *
;;;************************************

; ROOT
(defrule outdoor_check
    ; (system_banner)
    ?p <- (jeff (ID ?ID) (outdoor NONE))
    =>
    (modify ?p (outdoor (get_value "outdoor? ")))
)

; LEFT SIDE

; TBD

; RIGHT SIDE

; TBD

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system_banner 
    (declare (salience 10))
    =>
    (printout t "=====================================================" crlf)
    (printout t "+         Prediksi Attire Menggunakan CLIPS         +" crlf)
    (printout t "=====================================================" crlf))

(defrule print_result
    (patient (ID ?id) (result ?r))
    (not (eq ?r NONE))
    =>
    (if (eq ?r sportwear)           then (bind ?p "Sportwear"))
    (if (eq ?r pajamas)             then (bind ?p "Pajamas"))
    (if (eq ?r apron)               then (bind ?p "Apron"))
    (if (eq ?r tuxedo)              then (bind ?p "Tuxedo"))
    (if (eq ?r monochromatic_dress) then (bind ?p "Monochromatic dress"))
    (if (eq ?r cosplay_outfit)      then (bind ?p "Cosplay outfit"))
    (if (eq ?r halloween_outfit)    then (bind ?p "Halloween Outfit"))
    (if (eq ?r dresscode_outfit)    then (bind ?p "Dresscode outfit"))
    (if (eq ?r house_party_outfit)  then (bind ?p "House party outfit"))
    (if (eq ?r soft_fabric_attire)  then (bind ?p "Soft-fabric attire"))
    (if (eq ?r sweatshirt)          then (bind ?p "Sweatshirt"))
    (if (eq ?r business_attire)     then (bind ?p "Business attire"))
    (if (eq ?r uniform)             then (bind ?p "Uniform"))
    (if (eq ?r t_shirt_jeans)       then (bind ?p "T-shirt and jeans"))
    (if (eq ?r sportwear)           then (bind ?p "Sportwear"))
    (if (eq ?r swimsuit)            then (bind ?p "Swimsuit"))
    (if (eq ?r sportwear)           then (bind ?p "Sportwear"))
    (if (eq ?r raincoat)            then (bind ?p "Raincoat"))
    (if (eq ?r parka)               then (bind ?p "Parka"))
    (if (eq ?r sweater)             then (bind ?p "Sweater"))
    (printout t "Attire Jeff = " ?p crlf)
)