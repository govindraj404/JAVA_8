package com.sap.ic.cmh.masterdata.storagelocation.service;

import com.sap.cds.Result;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.plant.repository.PlantRepository;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationRequest;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationResponse;
import com.sap.ic.cmh.masterdata.storagelocation.repository.StorageLocationRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import cds.gen.masterdataservice.StorageLocations;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

@Service
public class StorageLocationServiceImpl implements StorageLocationService {

    public static final Logger logger = LoggerHelper.getLogger(StorageLocationServiceImpl.class);

    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired
    private StorageLocationRepository storageLocationRepository;
    @Autowired
    private PlantRepository plantRepository;

    @Override
    public List<StorageLocationResponse> deleteStorageLocationList(
            List<StorageLocationRequest> storageLocationRequestList) {
        LoggerHelper.logMethodEntry(logger, "StorageLocationServiceImpl",
                "deleteStorageLocationList");
        List<StorageLocationResponse> storageLocationResponseList = new ArrayList<>();
        AtomicInteger integerAtomic = new AtomicInteger();
        Map<String, String> storageLocationWithRecordNoMap = new HashMap<>();
        List<String> plantCodeList = new ArrayList<>();
        storageLocationRequestList.forEach(storageLocationRequest -> {
            storageLocationWithRecordNoMap.put(storageLocationRequest.getStorageLocation(),
                    String.valueOf(integerAtomic.incrementAndGet()));
            plantCodeList.add(storageLocationRequest.getPlantCode());
        });
        List<String> storageLocationList = new ArrayList<>(storageLocationWithRecordNoMap.keySet());
        final Map<String, String> storageLocationWithIdMap =
                storageLocationRepository.getStorageLocationMap(storageLocationList, plantCodeList);
        final Map<String, String> plantWithIdMap = plantRepository.getPlantMap(plantCodeList);
        storageLocationWithRecordNoMap.keySet().forEach(storageLocation -> {
            if (!storageLocationWithIdMap.containsValue(storageLocation)) {
                logger.info("Storage location Not Found in DB", storageLocation);
                StorageLocationResponse storageLocationResponse = new StorageLocationResponse();
                String recordNumber = storageLocationWithRecordNoMap.get(storageLocation);
                storageLocationResponse.setStorageLocation(storageLocation);
                storageLocationResponse.setMessage(
                        messageHelper.getMessage(MessageKeys.STORAGE_LOCATION_DOES_NOT_EXIST));
                storageLocationResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                storageLocationResponse.setRecordNo(recordNumber);
                storageLocationResponse
                        .setPlantCode(plantCodeList.get(Integer.parseInt(recordNumber) - 1));
                storageLocationResponseList.add(storageLocationResponse);
            }
        });
        if (!CollectionUtils.isEmpty(storageLocationWithIdMap)) {
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> plantIds = new ArrayList<>(plantWithIdMap.keySet());
            final List<String> plantsList = plantRepository.getActiveComplaintsInPlant(plantIds);
            for (Map.Entry<String, String> storageLocationWithId : storageLocationWithIdMap
                    .entrySet()) {
                StorageLocationResponse storageLocationResponse = new StorageLocationResponse();
                String storageLocation = storageLocationWithId.getValue();
                String recordNumber = storageLocationWithRecordNoMap.get(storageLocation);
                storageLocationResponse.setRecordNo(recordNumber);
                storageLocationResponse.setStorageLocation(storageLocation);
                String plantCode = plantCodeList.get(Integer.parseInt(recordNumber) - 1);
                String plantId = plantRepository.getPlantId(plantCode);
                storageLocationResponse.setPlantCode(plantCode);
                if (!plantsList.contains(plantId)) {
                    recordsToBeDeleted.add(storageLocationWithId.getKey());
                    storageLocationResponse.setMessage(messageHelper
                            .getMessage(MessageKeys.STORAGE_LOCATION_SUCCESSFULLY_DELETED));
                    storageLocationResponse
                            .setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                } else {
                    storageLocationResponse.setMessage(messageHelper
                            .getMessage(MessageKeys.STORAGE_LOCATION_ASSOCIATION_TO_COMPLAINT));
                    storageLocationResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                }
                storageLocationResponseList.add(storageLocationResponse);
            }
            if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                storageLocationRepository.deleteStorageLocationList(recordsToBeDeleted);
                logger.info("Records Deleted ", recordsToBeDeleted.size());
            }
        }
        return storageLocationResponseList;
    }

    @Override
    public StorageLocations fetchStorageLocations(String storageLocation, String plantId) {
        Result storageLocationResult =
                storageLocationRepository.fetchStorageLocations(storageLocation, plantId);
        return storageLocationResult.first().isPresent()
                ? storageLocationResult.listOf(StorageLocations.class).get(0)
                : null;
    }
}
