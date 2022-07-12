package com.sap.ic.cmh.masterdata.plant.validation;

import cds.gen.masterdataservice.Plants;

public interface PlantValidator {

    void checkInputsSanitized(Plants plant);
}