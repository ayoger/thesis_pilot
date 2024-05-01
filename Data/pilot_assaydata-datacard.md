---
language:
- en
tags:
- biology
- climate
- eelgrass
- San Francisco Bay
pretty_name: A. Valida Feeding Assay Eelgrass Scans - Pilot
---
# Dataset Card for pilot_assaydata

This dataset is from the SFSU Boyer Lab NSF Pilot project in Fall 2023.  

This dataset card has been generated using [this raw template](https://github.com/huggingface/huggingface_hub/blob/main/src/huggingface_hub/templates/datasetcard_template.md?plain=1).

## Dataset Details

### Dataset Description

In Fall 2023, the SFSU Boyer lab ran a pilot experiment at the Estuary and Ocean Science Center. The research objective was to determine the impacts of increased ocean acidity and nutrients on Ampithoe valida herbivory of eelgrass in San Francisco Bay. Mesocosm tanks were used to expose eelgrass plants to different treatment combinations in increased nutrients and ocean acidity for 30 days. A. valida were also exposed to either ambient bay water or acidified water. Then, eelgrass segments from plants from different treatment combinations were given to A.valida in two feeding assays, choice and no choice, to determine A.valida herbivory under the different treatments. Eelgrass segments were scanned pre and post feeding assays, and uploaded to ImageJ to determine the change is surface area of tissue as a a measure of herbivory. 

- **Curated by:** Amy Yoger
- **Funded by [optional]:** National Science Foundation
- **Language(s) (NLP):** English
- **License:** N/A

## Dataset Structure

### Dataset Fields

section_ID : text; shows the eelgrass section ID number; it can be read as tank#.plant#.segment#

area_pre	: numeric; surface area of eelgrass segment pre - assay

area_post	: numeric; surface area of eelgrass segment post - assay

area_change	: numeric; the change in surface area of the eelgrass segment 

scan_page_pre	: text; the ID of the scan page that the eelgrass was scanned on pre-assay

scan_page_post : text; the ID of the scan page that the eelgrass was scanned on post-assay

notes_pre	: text; informative notes on the scanning proccess 

notes_post: text; informative notes on the scanning proccess 

[image](https://github.com/ayoger/thesis_pilot/assets/142846984/2fd59246-01f6-4b51-815d-394ff294c00d)

## Dataset Card Authors 

Amy Yoger

## Dataset Card Contact

Amy Yoger
