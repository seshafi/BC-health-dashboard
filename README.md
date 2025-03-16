# British Columbia Mental Health and Substance Use Health Services

## Motivation

Target audience: Individuals seeking mental health services for themselves or a loved one

British Columbia has many mental health resources, but finding the right one can be overwhelming, especially in a crisis. This dashboard helps users locate nearby mental health and substance use services. Users can filter by service type and age group to find the right clinic for their needs. Clicking on a clinic provides key details on services provided, accessibility, hours of operation, language(s), and contact information.

## App description


## Installation Instructions

If you would like to run the app locally, please follow these instructions.

#### Create the Conda environment
Clone this repository locally in your desired location. Navigate to the root folder of the repository in terminal. Create the Conda environment with:
`conda env create -f environment.yml`

Activate the environment:
`conda activate BC_health`

#### Run the dashboard
With the environment activated and still in the root folder of the respository, type:
`RScript src/app.R`