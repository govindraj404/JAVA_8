package com.sap.ic.cmh.masterdata.plant.service;

import java.util.List;
import java.util.function.Function;
import com.sap.cds.ql.StructuredType;
import com.sap.ic.cmh.masterdata.plant.model.PlantRequest;
import com.sap.ic.cmh.masterdata.plant.model.PlantResponse;
import cds.gen.masterdataservice.Plants;

public interface PlantService {

    List<PlantResponse> deletePlantList(List<PlantRequest> plantListRequest);

    public <E extends StructuredType<E>> Plants fetchPlant(String plant, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

    public Plants fetchPlantDetailsBasedOnNumber(String plant);
}
