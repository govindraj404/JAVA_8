package com.sap.ic.cmh.masterdata.plant.repository;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import com.sap.cds.Result;
import com.sap.cds.ql.StructuredType;

public interface PlantRepository {
    Map<String, String> getPlantMap(List<String> plantList);

    List<String> getActiveComplaintsInPlant(List<String> plantsId);

    String getPlantId(String plantCode);

    void deletePlantList(List<String> recordsToBeDeleted);

    public <E extends StructuredType<E>> Result fetchPlant(String plant, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

    public Result fetchPlantDetailsBasedOnNumber(String plant);
}
