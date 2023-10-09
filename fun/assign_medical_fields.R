
## these functions categorise the journal fields, which are taken from the
## Scimago Journal & Country Rank (https://www.scimagojr.com/journalrank.php,
## downloaded on 12 September 2022) database, into mutually exclusive
## higher-order fields
## a complete list of categories can be found here:
## https://service.elsevier.com/app/answers/detail/a_id/15181/supporthub/scopus

## this function renames the journal names of the Scimago Journal & Country Rank
## database, to allow matching to our IntoValue dataset
rename_journals_for_matching <- function(x) {
  
  mutate(
    x,
    Title = str_replace(Title, '^lancet, the$', 'the lancet'),
    Title = str_replace(Title, '^lancet psychiatry,the$', 'the lancet psychiatry'),
    Title = str_replace(Title, '^lancet neurology, the$', 'the lancet neurology'),
    Title = str_replace(Title, '^lancet diabetes and endocrinology,the$', 'the lancet diabetes & endocrinology'),
    Title = str_replace(Title, '^lancet respiratory medicine,the$', 'the lancet respiratory medicine'),
    Title = str_replace(Title, '^lancet haematology,the$', 'the lancet haematology'),
    Title = str_replace(Title, '^lancet oncology, the$', 'the lancet oncology'),
    Title = str_replace(Title, '^lancet infectious diseases, the$', 'the lancet infectious diseases'),
    Title = str_replace(Title, '^bmj quality and safety$', 'bmj quality & safety'),
    Title = str_replace(Title, '^jama - journal of the american medical association$', 'jama'),
    Title = if_else(
      str_detect(Title, 'deutsches') & str_detect(Title, 'international'),
      'deutsches aerzteblatt online',
      Title
    ),
    Title = if_else(
      str_detect(Title, 'journal of dance medicine'),
      'journal of dance medicine & science',
      Title
    ),
    Title = str_replace(Title, '^journal of neuroscience$', 'the journal of neuroscience'),
    Title = str_replace(Title, '^gesundheitswesen$', 'das gesundheitswesen'),
    Title = str_replace(Title, '^journal of pain$', 'the journal of pain'),
    Title = str_replace(Title, '^canadian journal of psychiatry$', 'the canadian journal of psychiatry'),
    Title = str_replace(Title, '^journal of headache and pain$', 'the journal of headache and pain'),
    Title = str_replace(Title, '^international journal of prosthodontics$', 'the international journal of prosthodontics'),
    Title = str_replace(Title, '^journal of pediatrics$', 'the journal of pediatrics'),
    Title = str_replace(Title, '^laryngoscope$', 'the laryngoscope'),
    Title = str_replace(Title, '^klinische padiatrie$', 'klinische pädiatrie'),
    Title = str_replace(Title, '^human psychopharmacology$', 'human psychopharmacology: clinical and experimental'),
    Title = str_replace(Title, '^thoracic and cardiovascular surgeon$', 'the thoracic and cardiovascular surgeon'),
    Title = str_replace(Title, '^food and nutrition research$', 'food & nutrition research'),
    Title = str_replace(Title, "^alzheimer's research and therapy$", "alzheimer's research & therapy"),
    Title = str_replace(Title, '^klinische monatsblatter fur augenheilkunde$', 'klinische monatsblätter für augenheilkunde'),
    Title = str_replace(Title, '^scandinavian journal of medicine and science in sports$', 'scandinavian journal of medicine & science in sports'),
    Title = str_replace(Title, '^child abuse and neglect$', 'child abuse & neglect'),
    Title = str_replace(Title, '^anesthesia and analgesia$', 'anesthesia & analgesia'),
    Title = str_replace(Title, '^applied psychophysiology biofeedback$', 'applied psychophysiology and biofeedback'),
    Title = str_replace(Title, '^surgical endoscopy and other interventional techniques$', 'surgical endoscopy'),
    Title = str_replace(Title, '^acta dermato-venereologica$', 'acta dermato venereologica'),
    Title = str_replace(Title, '^respiratory physiology and neurobiology$', 'respiratory physiology & neurobiology'),
    Title = str_replace(Title, '^mental health and prevention$', 'mental health & prevention'),
    Title = str_replace(Title, '^applied physiology, nutrition and metabolism$', 'applied physiology, nutrition, and metabolism'),
    Title = str_replace(Title, '^journal of maternal-fetal and neonatal medicine$', 'the journal of maternal-fetal & neonatal medicine'),
    Title = str_replace(Title, '^archives of disease in childhood: fetal and neonatal edition$', 'archives of disease in childhood - fetal and neonatal edition'),
    Title = str_replace(Title, '^sportverletzung-sportschaden$', 'sportverletzung · sportschaden'),
    Title = str_replace(Title, '^neurology & neurosurgery$', 'neurology and neurosurgery'),
    Title = str_replace(Title, '^bmc complementary medicine and therapies$', 'bmc complementary and alternative medicine'),
    Title = str_replace(Title, '^british journal of radiology$', 'the british journal of radiology'),
    Title = str_replace(Title, '^health \\(united kingdom\\)$', 'health'),
    Title = str_replace(Title, '^physikalische medizin rehabilitationsmedizin kurortmedizin$', 'physikalische medizin, rehabilitationsmedizin, kurortmedizin'),
    Title = str_replace(Title, '^anxiety, stress and coping$', 'anxiety, stress & coping'),
    Title = str_replace(Title, '^journal of cranio-maxillo-facial surgery$', 'journal of cranio-maxillofacial surgery'),
    Title = str_replace(Title, '^clinical pharmacology and therapeutics$', 'clinical pharmacology & therapeutics'),
    Title = str_replace(Title, '^vasa - european journal of vascular medicine$', 'vasa'),
    Title = str_replace(Title, '^journal of neurology, neurosurgery and psychiatry$', 'journal of neurology, neurosurgery & psychiatry'),
    Title = str_replace(Title, '^annals of thoracic surgery$', 'the annals of thoracic surgery'),
    Title = str_replace(Title, '^kindheit und entwicklung \\(discontinued\\)$', 'kindheit und entwicklung'),
    Title = str_replace(Title, '^american journal of clinical nutrition$', 'the american journal of clinical nutrition'),
    Title = str_replace(Title, '^zeitschrift fur gerontologie und geriatrie$', 'zeitschrift für gerontologie und geriatrie'),
    Title = str_replace(Title, '^open nursing journal$', 'the open nursing journal'),
    Title = str_replace(Title, '^journal of heart and lung transplantation$', 'the journal of heart and lung transplantation'),
    Title = str_replace(Title, '^clinical the journal of pain$', 'the clinical journal of pain'),
    Title = str_replace(Title, '^international journal of radiation oncology biology physics$', 'international journal of radiation oncology*biology*physics'),
    Title = str_replace(Title, '^prostaglandins leukotrienes and essential fatty acids$', 'prostaglandins, leukotrienes and essential fatty acids'),
    Title = str_replace(Title, '^nutrition and metabolism$', 'nutrition & metabolism'),
    Title = str_replace(Title, '^journal of immunology$', 'the journal of immunology'),
    Title = str_replace(Title, '^ophthalmologe$', 'der ophthalmologe'),
    Title = str_replace(Title, '^journal of pediatric gastroenterology and nutrition$', 'journal of pediatric gastroenterology & nutrition'),
    Title = str_replace(Title, '^obstetrics and gynecology$', 'obstetrics & gynecology'),
    Title = str_replace(Title, '^gait and posture$', 'gait & posture'),
    Title = str_replace(Title, '^aging male$', 'the aging male'),
    Title = str_replace(Title, '^psychology and health$', 'psychology & health'),
    Title = str_replace(Title, '^zeitschrift fur evidenz, fortbildung und qualitat im gesundheitswesen$', 'zeitschrift für evidenz, fortbildung und qualität im gesundheitswesen'),
    Title = str_replace(Title, '^journal of nutrition$', 'the journal of nutrition'),
    Title = str_replace(Title, '^acta paediatrica, international journal of paediatrics$', 'acta paediatrica'),
    Title = str_replace(Title, '^deutsche zeitschrift fur sportmedizin$', 'deutsche zeitschrift für sportmedizin'),
    Title = str_replace(Title, '^ultraschall in der medizin$', 'ultraschall in der medizin - european journal of ultrasound'),
    Title = str_replace(Title, '^investigative ophthalmology and visual science$', 'investigative opthalmology & visual science'), # yes, this is written wrong in our dataset
    Title = str_replace(Title, '^herzschrittmachertherapie und elektrophysiologie$', 'herzschrittmachertherapie + elektrophysiologie'),
    Title = str_replace(Title, '^european journal of orthodontics$', 'the european journal of orthodontics'),
    Title = str_replace(Title, '^journal of laryngology and otology$', 'the journal of laryngology & otology'),
    Title = str_replace(Title, '^journal of bone and joint surgery - series a$', 'journal of bone and joint surgery'),
    Title = str_replace(Title, '^world journal of biological psychiatry$', 'the world journal of biological psychiatry'),
    Title = str_replace(Title, '^the international journal of cardiovascular imaging$', 'the international journal of cardiovascular imaging'),
    Title = str_replace(Title, '^ocular surface$', 'the ocular surface'),
    Title = str_replace(Title, '^bone and joint journal$', 'the bone & joint journal'),
    Title = str_replace(Title, "^alzheimer's and dementia: translational research and clinical interventions$", "alzheimer's & dementia: translational research & clinical interventions"),
    Title = str_replace(Title, '^american journal of cardiology$', 'the american journal of cardiology'),
    Title = str_replace(Title, '^clinical spine surgery$', 'clinical spine surgery: a spine publication'),
    Title = str_replace(Title, '^journal of sexual medicine$', 'the journal of sexual medicine'),
    Title = str_replace(Title, '^deutsche medizinische wochenschrift$', 'dmw - deutsche medizinische wochenschrift'),
    Title = str_replace(Title, '^international journal of neuropsychopharmacology$', 'the international journal of neuropsychopharmacology'),
    Title = str_replace(Title, "^canadian journal of anaesthesia$", "canadian journal of anesthesia/journal canadien d'anesthésie"),
    Title = str_replace(Title, '^clinical journal of the american society of nephrology : cjasn$', 'clinical journal of the american society of nephrology'),
    Title = str_replace(Title, '^leukemia and lymphoma$', 'leukemia & lymphoma'),
    Title = str_replace(Title, '^muscle and nerve$', 'muscle & nerve'),
    Title = str_replace(Title, '^clinical and experimental allergy$', 'clinical & experimental allergy'),
    Title = str_replace(Title, '^journal of thoracic and cardiovascular surgery$', 'the journal of thoracic and cardiovascular surgery'),
    Title = str_replace(Title, '^journal of clinical endocrinology and metabolism$', 'the journal of clinical endocrinology & metabolism'),
    Title = str_replace(Title, '^journal of the american academy of child and adolescent psychiatry$', 'journal of the american academy of child & adolescent psychiatry'),
    Title = str_replace(Title, '^otology and neurotology$', 'otology & neurotology'),
    Title = str_replace(Title, '^arthritis research and therapy$', 'arthritis research & therapy'),
    Title = str_replace(Title, '^nanomedicine: nanotechnology, biology, and medicine$', 'nanomedicine: nanotechnology, biology and medicine'),
    Title = str_replace(Title, '^european journal of surgical oncology$', 'european journal of surgical oncology (ejso)'),
    Title = str_replace(Title, '^journal of infectious diseases$', 'the journal of infectious diseases'),
    Title = str_replace(Title, "^journal of prevention of alzheimer's disease, the$", "the journal of prevention of alzheimer's disease"),
    Title = str_replace(Title, '^parkinsonism and related disorders$', 'parkinsonism & related disorders'),
    Title = str_replace(Title, '^neurogastroenterology and motility$', 'neurogastroenterology & motility'),
    Title = str_replace(Title, '^seizure : the journal of the british epilepsy association$', 'seizure'),
    Title = str_replace(Title, '^american journal of rhinology and allergy$', 'american journal of rhinology & allergy'),
    Title = str_replace(Title, '^alimentary pharmacology and therapeutics$', 'alimentary pharmacology & therapeutics'),
    Title = str_replace(Title, '^allergy: european journal of allergy and clinical immunology$', 'allergy'),
    Title = str_replace(Title, '^proceedings of the national academy of sciences of the united states of america$', 'proceedings of the national academy of sciences'),
    Title = str_replace(Title, '^arthritis and rheumatology$', 'arthritis & rheumatism'),
    Title = str_replace(Title, '^pediatric blood and cancer$', 'pediatric blood & cancer'),
    Title = str_replace(Title, '^journal of acquired immune deficiency syndromes \\(1999\\)$', 'jaids journal of acquired immune deficiency syndromes'),
    Title = str_replace(Title, '^hiv research and clinical practice$', 'hiv clinical trials'), # journal was renamed, apparently
    Title = str_replace(Title, '^paediatric anaesthesia$', 'pediatric anesthesia'),
    Title = str_replace(Title, '^international journal of cardiovascular imaging$', 'the international journal of cardiovascular imaging'),
    Title = str_replace(Title, '^metabolism: clinical and experimental$', 'metabolism'),
    Title = str_replace(Title, '^arthritis and rheumatology$', 'arthritis & rheumatism'),
    Title = str_replace(Title, '^arthritis & rheumatology$', 'arthritis & rheumatism'),
    Title = str_replace(Title, '^clinical psychology and psychotherapy$', 'clinical psychology & psychotherapy'),
    Title = str_replace(Title, '^telemedicine journal and e-health$', 'telemedicine and e-health'), # name not the same, but ISSN does match!
    Title = str_replace(Title, '^european journal of obstetrics, gynecology and reproductive biology$', 'european journal of obstetrics & gynecology and reproductive biology'),
    Title = str_replace(Title, '^journal of alternative and complementary medicine$', 'the journal of alternative and complementary medicine'),
    Title = str_replace(Title, '^international journal of copd$', 'international journal of chronic obstructive pulmonary disease'),
    Title = str_replace(Title, '^clinical journal of pain$', 'the clinical journal of pain'),
    Title = str_replace(Title, '^zeitschrift fur psychiatrie, psychologie und psychotherapie$', 'zeitschrift für psychiatrie, psychologie und psychotherapie'),
    Title = str_replace(Title, '^journals of gerontology - series a biological sciences and medical sciences$', 'the journals of gerontology: series a'),
    Title = str_replace(Title, '^journal of clinical pharmacology$', 'the journal of clinical pharmacology'),
    Title = str_replace(Title, '^molecular nutrition and food research$', 'molecular nutrition & food research'),
    Title = str_replace(Title, '^medicine and science in sports and exercise$', 'medicine & science in sports & exercise'),
    Title = str_replace(Title, '^perfusion \\(united kingdom\\)$', 'perfusion'),
    Title = str_replace(Title, '^journal of gastroenterology and hepatology \\(australia\\)$', 'journal of gastroenterology and hepatology'),
    Title = str_replace(Title, '^american journal of obstetrics & gynecology$', 'american journal of obstetrics and gynecology'),
    Title = str_replace(Title, '^journal of child psychology and psychiatry and allied disciplines$', 'journal of child psychology and psychiatry'), # match identified by ISSM
    Title = str_replace(Title, '^zeitschrift fur phytotherapie : offizielles organ der ges. f. phytotherapie e.v$', 'phytotherapie 2017 „von der innovation zur evidenz“') # identified via web search and ISSN
  )
  
  ## for the following journals, the doi was checked, but no matching journal name
  ## or ISSN was found in the Scimago database:
  ## open journal of thoracic surgery
  ## journal of obesity and bariatrics
  ## journal of obesity & weight loss therapy
  ## neurology and neurosurgery
  ## world journal of radiology
  ## gynecologic oncology research and practice (--> this is a BMC journal that was discontinued)
  ## clinical and vaccine immunology (--> journal was discontinued)
  ## zwr - das deutsche zahnärzteblatt (--> journal was discontinued in 2014)
  ## age (--> seems to have been renamed to GeroScience, but still not found)
  ## brazilian journal of anesthesiology (english edition)
  ## the open orthopaedics journal
  ## endoscopy international open
  
}



## this function harmonises the journal names in our IntoValue dataset, as some
## of them have multiple spellings, which makes matching to the Scimago Journal
## & Country Rank database more difficult
harmonise_journal_names <- function(x) {
  
  mutate(
    x,
    journal_name_matching = str_replace(journal_name_matching, '^bmj$', 'bmj, the'),
    journal_name_matching = str_replace(journal_name_matching, '^anxiety, stress, & coping$', 'anxiety, stress & coping'),
    journal_name_matching = str_replace(journal_name_matching, '^archives of general psychiatry$', 'jama psychiatry'),
    # (the Archives of General Psychiatry now seem to have become JAMA Psychiatry)
    journal_name_matching = str_replace(journal_name_matching, '^international journal of neuropsychopharmacology$', 'the international journal of neuropsychopharmacology'),
    journal_name_matching = str_replace(journal_name_matching, '^arthritis & rheumatology$', 'arthritis & rheumatism')
  )
  
}



## this function just creates a binary variable inside a dataframe 'x'
categorisefields_binary <- function(x) {
  mutate(
    x,
    medical_field_recoded_binary = case_when(
      str_detect(medical_field, 'Multidisciplinary') |
        str_detect(medical_field, 'Medicine \\(miscellaneous\\)') |
        str_detect(medical_field, 'Reviews and References \\(medical\\)') |
        str_detect(medical_field, 'General Nursing') |
        str_detect(medical_field, 'General Pharmacology, Toxicology and Pharmaceutics') |
        str_detect(medical_field, 'General Psychology') |
        str_detect(medical_field, 'General Dentistry') |
        str_detect(medical_field, 'General Health Professions') ~
      'General',
      TRUE ~ 'Specialty'
    )
  )
}



## this function creates a more fine-grained classification inside a dataframe 'x'
categorisefields <- function(x) {
  mutate(
    x,
    medical_field_recoded = case_when(
      
      # General Medicine
      str_detect(medical_field, 'Multidisciplinary') |
        str_detect(medical_field, 'Medicine \\(miscellaneous\\)') |
        str_detect(medical_field, 'Reviews and References \\(medical\\)') ~
        'General Medicine',
      
      # Surgery
      # (this category is based on parts of Scopus category 27: Medicine)
      str_detect(medical_field, 'Orthopedics and Sports Medicine') |
        str_detect(medical_field, 'Surgery') ~
        'Surgery',
      
      # Internal Medicine
      # (this category is based on parts of Scopus category 27: Medicine)
      str_detect(medical_field, 'Cardiology and Cardiovascular Medicine') |
        str_detect(medical_field, 'Endocrinology, Diabetes and Metabolism') |
        str_detect(medical_field, 'Gastroenterology') |
        str_detect(medical_field, 'Geriatrics and Gerontology') |
        str_detect(medical_field, 'Hematology') |
        str_detect(medical_field, 'Hepatology') |
        str_detect(medical_field, 'Immunology and Allergy') |
        str_detect(medical_field, 'Internal Medicine') |
        str_detect(medical_field, 'Nephrology') |
        str_detect(medical_field, 'Pulmonary and Respiratory Medicine') |
        str_detect(medical_field, 'Rheumatology') |
        str_detect(medical_field, 'Transplantation') ~
        'Internal Medicine',
      
      # Oncology
      # (this category is based on parts of Scopus category 27: Medicine)
      str_detect(medical_field, 'Oncology') ~
        'Oncology',
      
      # Family & Reproductive Medicine
      # (this category is based on parts of Scopus category 27: Medicine)
      str_detect(medical_field, 'Family Practice') |
        str_detect(medical_field, 'Obstetrics and Gynecology') |
        str_detect(medical_field, 'Pediatrics, Perinatology and Child Health') |
        str_detect(medical_field, 'Urology') ~
        'Family & Reproductive Medicine',
      
      # Other Clinical Fields
      # (this category is based on parts of Scopus category 27: Medicine)
      str_detect(medical_field, 'Dermatology') |
        str_detect(medical_field, 'Embryology') |
        str_detect(medical_field, 'Emergency Medicine') |
        str_detect(medical_field, 'Ophthalmology') |
        str_detect(medical_field, 'Otorhinolaryngology') |
        str_detect(medical_field, 'Reproductive Medicine') ~
        'Other Clinical Field',
      
      # Public Health
      # (this category is based on parts of Scopus category 27: Medicine)
      str_detect(medical_field, 'Epidemiology') |
        str_detect(medical_field, 'Public Health, Environmental and Occupational Health') ~
        'Epidemiology and Public Health',

      # Other Medical Fields
      # (this category is based on parts of Scopus category 27: Medicine)
      str_detect(medical_field, 'Anatomy') |
        str_detect(medical_field, 'Health Informatics') |
        str_detect(medical_field, 'Health Policy') |
        str_detect(medical_field, 'Histology') |
        str_detect(medical_field, 'Pathology and Forensic Medicine') |
        str_detect(medical_field, 'Radiology, Nuclear Medicine and Imaging') |
        str_detect(medical_field, 'Rehabilitation') ~
        'Other Medical Field',
      
      # Neuroscience
      # (unless noted, this category is based on Scopus category 28: Neuroscience)
      str_detect(medical_field, 'General Neuroscience') |
        str_detect(medical_field, 'Neuroscience \\(miscellaneous\\)') |
        str_detect(medical_field, 'Behavioral Neuroscience') |
        str_detect(medical_field, 'Biological Psychiatry') |
        str_detect(medical_field, 'Cellular and Molecular Neuroscience') |
        str_detect(medical_field, 'Cognitive Neuroscience') |
        str_detect(medical_field, 'Developmental Neuroscience') |
        str_detect(medical_field, 'Endocrine and Autonomic Systems') |
        str_detect(medical_field, 'Neurology') |
        str_detect(medical_field, 'Sensory Systems') |
        str_detect(medical_field, 'Neurology \\(clinical\\)') ~ # (this is from the Scopus 'Medicine' category 27)
        'Neuroscience',
      
      # Psychology
      # (unless noted, this category is based on Scopus category 32: Psychology)
      str_detect(medical_field, 'General Psychology') |
        str_detect(medical_field, 'Psychology \\(miscellaneous\\)') |
        str_detect(medical_field, 'Applied Psychology') |
        str_detect(medical_field, 'Clinical Psychology') |
        str_detect(medical_field, 'Developmental and Educational Psychology') |
        str_detect(medical_field, 'Experimental and Cognitive Psychology') |
        str_detect(medical_field, 'Neuropsychology and Physiological Psychology') |
        str_detect(medical_field, 'Social Psychology') |
        str_detect(medical_field, 'General Social Sciences') | # (this is from the Scopus 'Social Sciences' category 33)
        str_detect(medical_field, 'Social Sciences \\(miscellaneous\\)') | # (this is from the Scopus 'Social Sciences' category 33)
        str_detect(medical_field, 'Health \\(social science\\)') | # (this is from the Scopus 'Social Sciences' category 33)
        str_detect(medical_field, 'Human Factors and Ergonomics') | # (this is from the Scopus 'Social Sciences' category 33)
        str_detect(medical_field, 'Psychiatry and Mental Health') ~ # (this is from the Scopus 'Medicine' category 27)
        'Psychology and Psychiatry',
      
      # Pharmacology, Toxicology and Pharmaceutics
      # (unless noted, this category is based on Scopus category 30: Pharmacology, Toxicology and Pharmaceutics)
      str_detect(medical_field, 'General Pharmacology, Toxicology and Pharmaceutics') |
        str_detect(medical_field, 'Pharmacology, Toxicology and Pharmaceutics \\(miscellaneous\\)') |
        str_detect(medical_field, 'Drug Discovery') |
        str_detect(medical_field, 'Pharmaceutical Science') |
        str_detect(medical_field, 'Pharmacology') |
        str_detect(medical_field, 'Toxicology') |
        str_detect(medical_field, 'Pharmacology \\(medical\\)') | # (this is from the Scopus 'Medicine' category 27)
        str_detect(medical_field, 'Anesthesiology and Pain Medicine') | # (this is from the Scopus 'Medicine' category 27)
        str_detect(medical_field, 'Critical Care and Intensive Care Medicine') | # (this is from the Scopus 'Medicine' category 27)
        str_detect(medical_field, 'Drug Guides') ~ # (this is from the Scopus 'Medicine' category 27)
        'Pharmacology, Toxicology and Pharmaceutics',
      
      # Immunology and Microbiology
      # (unless noted, this category is based on Scopus category 24: Immunology and Microbiology)
      str_detect(medical_field, 'General Immunology and Microbiology') |
        str_detect(medical_field, 'Immunology and Microbiology \\(miscellaneous\\)') |
        str_detect(medical_field, 'Applied Microbiology and Biotechnology') |
        str_detect(medical_field, 'Immunology') |
        str_detect(medical_field, 'Microbiology') |
        str_detect(medical_field, 'Parasitology') |
        str_detect(medical_field, 'Virology') |
        str_detect(medical_field, 'Microbiology \\(medical\\)') | # (this is from the Scopus 'Medicine' category 27)
        str_detect(medical_field, 'Infectious Diseases') ~ # (this is from the Scopus 'Medicine' category 27)
        'Immunology and Microbiology',
      
      # Nursing
      # (this category is based on Scopus category 29: Nursing)
      str_detect(medical_field, 'General Nursing') |
        str_detect(medical_field, 'Nursing \\(miscellaneous\\)') |
        str_detect(medical_field, 'Advanced and Specialized Nursing') |
        str_detect(medical_field, 'Assessment and Diagnosis') |
        str_detect(medical_field, 'Care Planning') |
        str_detect(medical_field, 'Community and Home Care') |
        str_detect(medical_field, 'Critical Care Nursing') |
        str_detect(medical_field, 'Emergency Nursing') |
        str_detect(medical_field, 'Fundamentals and Skills') |
        str_detect(medical_field, 'Gerontology') |
        str_detect(medical_field, 'Issues, Ethics and Legal Aspects') |
        str_detect(medical_field, 'Leadership and Management') |
        str_detect(medical_field, 'LPN and LVN') |
        str_detect(medical_field, 'Maternity and Midwifery') |
        str_detect(medical_field, 'Medical and Surgical Nursing') |
        str_detect(medical_field, 'Nurse Assisting') |
        str_detect(medical_field, 'Nutrition and Dietetics') |
        str_detect(medical_field, 'Oncology \\(nursing\\)') |
        str_detect(medical_field, 'Pathophysiology') |
        str_detect(medical_field, 'Pediatrics') |
        str_detect(medical_field, 'Pharmacology \\(nursing\\)') |
        str_detect(medical_field, 'Psychiatric Mental Health') |
        str_detect(medical_field, 'Research and Theory') |
        str_detect(medical_field, 'Review and Exam Preparation') ~
        'Nursing',
      
      # Dentistry
      # (this category is based on Scopus category 35: Dentistry)
      str_detect(medical_field, 'General Dentistry') |
        str_detect(medical_field, 'Dentistry \\(miscellaneous\\)') |
        str_detect(medical_field, 'Dental Assisting') |
        str_detect(medical_field, 'Dental Hygiene') |
        str_detect(medical_field, 'Oral Surgery') |
        str_detect(medical_field, 'Orthodontics') |
        str_detect(medical_field, 'Periodontics') ~
        'Dentistry',
      
      # Basic Research
      # (unless noted, this category is based on Scopus category 13: Biochemistry, Genetics and Molecular Biology)
      str_detect(medical_field, 'General Biochemistry, Genetics and Molecular Biology') |
        str_detect(medical_field, 'Biochemistry, Genetics and Molecular Biology \\(miscellaneous\\)') |
        str_detect(medical_field, 'Aging') |
        str_detect(medical_field, 'Biochemistry') |
        str_detect(medical_field, 'Biophysics') |
        str_detect(medical_field, 'Biotechnology') |
        str_detect(medical_field, 'Cancer Research') |
        str_detect(medical_field, 'Cell Biology') |
        str_detect(medical_field, 'Clinical Biochemistry') |
        str_detect(medical_field, 'Developmental Biology') |
        str_detect(medical_field, 'Endocrinology') |
        str_detect(medical_field, 'Genetics') |
        str_detect(medical_field, 'Molecular Biology') |
        str_detect(medical_field, 'Molecular Medicine') |
        str_detect(medical_field, 'Physiology') |
        str_detect(medical_field, 'Structural Biology') |
        str_detect(medical_field, 'Bioengineering') | # (this is from the Scopus 'Chemical Engineering' category 15)
        str_detect(medical_field, 'Biomedical Engineering') | # (this is from the Scopus 'Engineering' category 22)
        str_detect(medical_field, 'Biomaterials') | # (this is from the Scopus 'Materials Science' category 25)
        str_detect(medical_field, 'Biochemistry \\(medical\\)') | # (this is from the Scopus 'Medicine' category 27)
        str_detect(medical_field, 'Genetics \\(clinical\\) ') | # (this is from the Scopus 'Medicine' category 27)
        str_detect(medical_field, 'Physiology \\(medical\\)') ~ # (this is from the Scopus 'Medicine' category 27)
        'Basic',
      
      # Health Professions
      # (unless noted, this category is based on Scopus category 36: Health Professions)
      str_detect(medical_field, 'General Health Professions') |
        str_detect(medical_field, 'Health Professions \\(miscellaneous\\)') |
        str_detect(medical_field, 'Chiropractics') |
        str_detect(medical_field, 'Complementary and Manual Therapy') |
        str_detect(medical_field, 'Emergency Medical Services') |
        str_detect(medical_field, 'Health Information Management') |
        str_detect(medical_field, 'Medical Assisting and Transcription') |
        str_detect(medical_field, 'Medical Laboratory Technology') |
        str_detect(medical_field, 'Medical Terminology') |
        str_detect(medical_field, 'Occupational Therapy') |
        str_detect(medical_field, 'Optometry') |
        str_detect(medical_field, 'Pharmacy') |
        str_detect(medical_field, 'Physical Therapy, Sports Therapy and Rehabilitation') |
        str_detect(medical_field, 'Podiatry') |
        str_detect(medical_field, 'Radiological and Ultrasound Technology') |
        str_detect(medical_field, 'Respiratory Care') |
        str_detect(medical_field, 'Speech and Hearing') |
        str_detect(medical_field, 'Complementary and Alternative Medicine') ~ # (this is from the Scopus 'Medicine' category 27)
        'Health Professions',
      
      # Others, which are mostly trials with no matched journal name
      TRUE ~ 'Other / No medical field assigned'
    )
  )
}
