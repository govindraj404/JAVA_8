package com.sap.ic.cmh.masterdata.plant.service;

import com.sap.cds.Result;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.plant.model.PlantRequest;
import com.sap.ic.cmh.masterdata.plant.model.PlantResponse;
import com.sap.ic.cmh.masterdata.plant.repository.PlantRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import cds.gen.masterdataservice.Plants;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

@Service
public class PlantServiceImpl implements PlantService {
    public static final Logger logger = LoggerHelper.getLogger(PlantServiceImpl.class);
    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired
    private PlantRepository plantRepository;
    @Autowired
    Messages messages;

    @Override
    public List<PlantResponse> deletePlantList(List<PlantRequest> plantListRequest) {
        LoggerHelper.logMethodEntry(logger, "PlantServiceImpl", "deletePlantList");
        List<PlantResponse> plantErrorRecsResponseList = new ArrayList<>();
        AtomicInteger integerAtomic = new AtomicInteger();
        Map<String, String> plantWithRecordNoMap = new HashMap<>();
        plantListRequest.forEach(plantRequest ->
                plantWithRecordNoMap.put(plantRequest.getPlant(), String.valueOf(integerAtomic.incrementAndGet())));
        List<String> plantList = new ArrayList<>(plantWithRecordNoMap.keySet());
        final Map<String, String> plantWithIdMap = plantRepository.getPlantMap(plantList);
        plantWithRecordNoMap.keySet().forEach(plant -> {
            if (!plantWithIdMap.containsValue(plant)) {
                logger.info("Plant Not Found in DB", plant);
                PlantResponse plantDataResponse = new PlantResponse();
                plantDataResponse.setPlant(plant);
                plantDataResponse.setMessage(messageHelper.getMessage(MessageKeys.PLANT_DOES_NOT_EXIST));
                plantDataResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                plantDataResponse.setRecordNo(plantWithRecordNoMap.get(plant));
                plantErrorRecsResponseList.add(plantDataResponse);
            }
        });
        if (!CollectionUtils.isEmpty(plantWithIdMap)) {
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> plantsId = new ArrayList<>(plantWithIdMap.keySet());
            List<String> complaintPlantDbList = plantRepository.getActiveComplaintsInPlant(plantsId);
            for (String plantId : plantsId) {
                PlantResponse plantDataResponse = new PlantResponse();
                String plant = plantWithIdMap.get(plantId);
                plantDataResponse.setPlant(plant);
                plantDataResponse.setRecordNo(plantWithRecordNoMap.get(plant));
                if (!complaintPlantDbList.contains(plantId)) {
                    recordsToBeDeleted.add(plantId);
                    plantDataResponse.setMessage(messageHelper.getMessage(MessageKeys.PLANT_SUCCESSFULLY_DELETED));
                    plantDataResponse.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                } else {
                    plantDataResponse.setMessage(messageHelper.getMessage(MessageKeys.PLANT_ASSOCIATION_TO_COMPLAINT));
                    plantDataResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                }
                plantErrorRecsResponseList.add(plantDataResponse);
            }
            if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                plantRepository.deletePlantList(recordsToBeDeleted);
                logger.info("Records Deleted ", recordsToBeDeleted.size());
            }
        }
        return plantErrorRecsResponseList;
    }

    @Override
    public <E extends StructuredType<E>> Plants fetchPlant(String plant, String message,
            Class<E> targetClass, Function<E, Object> targetClassAttribute) {
                Plants plants = null;
     Result plantResult =  plantRepository.fetchPlant(plant, message, targetClass, targetClassAttribute);
      if (plantResult != null && plantResult.first(Plants.class).isPresent()) {
                plants = plantResult.first(Plants.class).get();
            } else {
                messages.error(message).target("in", targetClass, targetClassAttribute);
             }
    return plants;
    }

    @Override
    public Plants fetchPlantDetailsBasedOnNumber(String plant) {
        Result plantResult =  plantRepository.fetchPlantDetailsBasedOnNumber(plant);
        return plantResult.first().isPresent() ? plantResult.listOf(Plants.class).get(0): null;
    }
}
