
## these functions categorise the journal fields, which are taken from the
## Scimago Journal & Country Rank (https://www.scimagojr.com/journalrank.php,
## downloaded on 12 September 2022) database, into mutually exclusive
## higher-order fields
## a complete list of categories can be found here:
## https://service.elsevier.com/app/answers/detail/a_id/15181/supporthub/scopus


## this function just creates a binary variable inside a dataframe 'x'
categorisefields_binary <- function(x) {
  mutate(
    x,
    medical_field_recoded_binary = if_else(
      str_detect(medical_field, 'Multidisciplinary') |
        str_detect(medical_field, 'Medicine \\(miscellaneous\\)') |
        str_detect(medical_field, 'Reviews and References \\(medical\\)') |
        str_detect(medical_field, 'General Nursing') |
        str_detect(medical_field, 'General Pharmacology, Toxicology and Pharmaceutics') |
        str_detect(medical_field, 'General Psychology') |
        str_detect(medical_field, 'General Dentistry') |
        str_detect(medical_field, 'General Health Professions'),
      'General',
      'Specialty'
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
      
      # Others
      TRUE ~ 'Other'
    )
  )
}
