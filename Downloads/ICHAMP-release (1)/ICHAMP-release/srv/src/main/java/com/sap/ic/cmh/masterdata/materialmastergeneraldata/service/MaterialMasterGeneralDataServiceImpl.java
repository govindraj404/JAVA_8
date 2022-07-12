package com.sap.ic.cmh.masterdata.materialmastergeneraldata.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataRequest;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataResponse;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository.MaterialMasterGeneralDataRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;

@Service
public class MaterialMasterGeneralDataServiceImpl implements MaterialMasterGeneralDataService {

    public static final Logger logger =
            LoggerHelper.getLogger(MaterialMasterGeneralDataServiceImpl.class);
    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired
    private MaterialMasterGeneralDataRepository materialMasterGeneralDataRepository;
    @Autowired
    Messages messages;

    @Override
    public List<MaterialMasterGeneralDataResponse> deleteMaterialMasterGeneralDataList(
            List<MaterialMasterGeneralDataRequest> materialMasterGeneralDataRequest) {
        LoggerHelper.logMethodEntry(logger, "MaterialMasterGeneralDataServiceImpl",
                "deleteMaterialMasterGeneralDataList");
        List<MaterialMasterGeneralDataResponse> materialMasterGeneralDataResponseList =
                new ArrayList<>();
        AtomicInteger index = new AtomicInteger();
        Map<String, String> materialMasterGeneralDatasWithRecordNoMap = new HashMap<>();
        materialMasterGeneralDataRequest
                .forEach(materialMasterGeneralData -> materialMasterGeneralDatasWithRecordNoMap.put(
                        materialMasterGeneralData.getMaterialCode(),
                        String.valueOf(index.incrementAndGet())));
        List<String> materialMasterGeneralDataList =
                new ArrayList<>(materialMasterGeneralDatasWithRecordNoMap.keySet());
        Map<String, String> materialMasterGeneralDataMapWithId = materialMasterGeneralDataRepository
                .getMaterialMasterGeneralDataMap(materialMasterGeneralDataList);
        materialMasterGeneralDatasWithRecordNoMap.keySet().forEach(materialMasterGeneralData -> {
            if (!materialMasterGeneralDataMapWithId.containsValue(materialMasterGeneralData)) {
                logger.info("Master General Data Not Found in DB", materialMasterGeneralData);
                MaterialMasterGeneralDataResponse materialMasterGeneralDataResponse =
                        new MaterialMasterGeneralDataResponse();
                materialMasterGeneralDataResponse.setMaterialCode(materialMasterGeneralData);
                materialMasterGeneralDataResponse.setMessage(messageHelper
                        .getMessage(MessageKeys.MATERIAL_MASTER_GENERAL_DATA_DOES_NOT_EXIST));
                materialMasterGeneralDataResponse
                        .setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                materialMasterGeneralDataResponse.setRecordNo(
                        materialMasterGeneralDatasWithRecordNoMap.get(materialMasterGeneralData));
                materialMasterGeneralDataResponseList.add(materialMasterGeneralDataResponse);
            }
        });
        if (!materialMasterGeneralDataMapWithId.isEmpty()) {
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> materialMasterGeneralDataIdList =
                    new ArrayList<>(materialMasterGeneralDataMapWithId.keySet());
            List<String> complaintMaterialMasterGeneralDataDbList =
                    materialMasterGeneralDataRepository
                            .getActiveComplaintsInMaterialMasterGeneralData(
                                    materialMasterGeneralDataIdList);
            materialMasterGeneralDataIdList.forEach(materialMasterGeneralDataId -> {
                MaterialMasterGeneralDataResponse materialMasterGeneralDataResponse =
                        new MaterialMasterGeneralDataResponse();
                materialMasterGeneralDataResponse.setMaterialCode(
                        materialMasterGeneralDataMapWithId.get(materialMasterGeneralDataId));
                materialMasterGeneralDataResponse
                        .setRecordNo(materialMasterGeneralDatasWithRecordNoMap
                                .get(materialMasterGeneralDataResponse.getMaterialCode()));
                if (!complaintMaterialMasterGeneralDataDbList
                        .contains(materialMasterGeneralDataId)) {
                    // Collect records to be deleted
                    recordsToBeDeleted.add(materialMasterGeneralDataId);
                    materialMasterGeneralDataResponse.setMessage(messageHelper.getMessage(
                            MessageKeys.MATERIAL_MASTER_GENERAL_DATA_SUCCESSFULLY_DELETED));
                    materialMasterGeneralDataResponse
                            .setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                } else {
                    logger.info("Complaints for ",
                            materialMasterGeneralDataMapWithId.get(materialMasterGeneralDataId)
                                    + " is not active ");
                    // Collect the error records to be sent to the response
                    materialMasterGeneralDataResponse.setMessage(messageHelper.getMessage(
                            MessageKeys.MATERIAL_MASTER_GENERAL_DATA_ASSOCIATION_TO_COMPLAINT));
                    materialMasterGeneralDataResponse
                            .setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                }
                materialMasterGeneralDataResponseList.add(materialMasterGeneralDataResponse);
            });
            if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                materialMasterGeneralDataRepository
                        .deleteMaterialMasterGeneralDataList(recordsToBeDeleted);
                logger.info("Records Deleted ", recordsToBeDeleted.size());
            }
        }
        return materialMasterGeneralDataResponseList;
    }

    @Override
    public <E extends StructuredType<E>> MaterialMasterGeneralDatas fetchMaterialMasterGeneralData(
            String materialMasterGeneralData, String message, Class<E> targetClass,
            Function<E, Object> targetClassAttribute) {
        MaterialMasterGeneralDatas materialMasterGeneralDatas = null;
        Result materialDataResult =
                materialMasterGeneralDataRepository.fetchMaterialMasterGeneralData(
                        materialMasterGeneralData, message, targetClass, targetClassAttribute);
        if (materialDataResult != null
                && materialDataResult.first(MaterialMasterGeneralDatas.class).isPresent()) {
            materialMasterGeneralDatas =
                    materialDataResult.first(MaterialMasterGeneralDatas.class).get();
        } else {
            messages.error(message).target("in", targetClass, targetClassAttribute);
        }
        return materialMasterGeneralDatas;

    }

    @Override
    public MaterialMasterGeneralDatas fetchMaterialMasterGeneralDataBasedOnCode(
            String materialCode) {
        Result masterDataResult = materialMasterGeneralDataRepository
                .fetchMaterialMasterGeneralDataBasedOnCode(materialCode);
        return masterDataResult.first().isPresent()
                ? masterDataResult.listOf(MaterialMasterGeneralDatas.class).get(0)
                : null;
    }
}
