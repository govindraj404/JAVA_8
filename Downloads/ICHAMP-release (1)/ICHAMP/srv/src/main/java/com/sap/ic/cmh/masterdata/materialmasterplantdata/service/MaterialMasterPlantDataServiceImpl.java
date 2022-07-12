package com.sap.ic.cmh.masterdata.materialmasterplantdata.service;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterPlantDatas;
import com.sap.cds.Result;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository.MaterialMasterGeneralDataRepository;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataRequest;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataResponse;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.repository.MaterialMasterPlantDataRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Service
public class MaterialMasterPlantDataServiceImpl implements MaterialMasterPlantDataService {
    public static final Logger logger = LoggerHelper.getLogger(MaterialMasterPlantDataServiceImpl.class);
    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired
    private MaterialMasterPlantDataRepository materialMasterPlantDataRepository;
    @Autowired
    private MaterialMasterGeneralDataRepository materialMasterGeneralDataRepository;

    /**
     * This will delete the plant inactive records by validating with the material master, complaints and send
     * the records which are not deleted back to the response
     *
     * @param materialMasterPlantDataRequest {@link MaterialMasterPlantDataRequest}
     * @return MaterialMasterPlantDataResponse response
     */
    @Override
    public List<MaterialMasterPlantDataResponse> deleteMaterialMasterPlantDataList(List<MaterialMasterPlantDataRequest> materialMasterPlantDataRequest) {
        LoggerHelper.logMethodEntry(logger, "MaterialMasterPlantDataServiceImpl", "deleteMaterialMasterGeneralDataList");
        List<MaterialMasterPlantDataResponse> materialMasterPlantDataResponseList = new ArrayList<>();
        Map<String, MaterialMasterPlantDataRequest> masterPlantDataRequestMap = new LinkedHashMap<>();
        List<String> materialCodes = new ArrayList<>();
        List<String> plantList = new ArrayList<>();
        AtomicInteger integerAtomic = new AtomicInteger();
        materialMasterPlantDataRequest.forEach(materialMasterPlantData -> {
            materialMasterPlantData.setRecordNo(String.valueOf(integerAtomic.getAndIncrement()));
            materialCodes.add(materialMasterPlantData.getMaterialCode());
            plantList.add(materialMasterPlantData.getPlant());
            masterPlantDataRequestMap.put(materialMasterPlantData.getMaterialCode(), materialMasterPlantData);
        });
        Map<String, String> materialMasterDataDB = materialMasterPlantDataRepository.getMaterialMasterPlantDataMap(materialCodes, plantList);
        logger.info("Material fetched From DB:", materialMasterDataDB.size());
        masterPlantDataRequestMap.forEach((materialCode, materialMasterPlantData) -> {
            if (!materialMasterDataDB.containsKey(materialCode)) {
                logger.info("Value Not Picked from DB:", materialCode);
                MaterialMasterPlantDataResponse response = new MaterialMasterPlantDataResponse();
                response.setMaterialCode(materialCode);
                response.setMessage(messageHelper.getMessage(MessageKeys.MATERIAL_MASTER_PLANT_DATA_DOES_NOT_EXIST));
                response.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                response.setRecordNo(materialMasterPlantData.getRecordNo());
                materialMasterPlantDataResponseList.add(response);
            }
        });
        //Check for active complaint records
        if (!materialMasterDataDB.isEmpty()) {
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> materialCodeList = new ArrayList<>(materialMasterDataDB.keySet());
            final List<MaterialMasterGeneralDatas> materialMasterGeneralDataList = materialMasterPlantDataRepository.getMaterialMasterGeneralDataList(materialCodeList);
            List<String> materialDBIDs = materialMasterGeneralDataList.stream().map(MaterialMasterGeneralDatas::getId).collect(Collectors.toList());
            final List<String> complaintMaterialMasterGeneralDataDbList = materialMasterGeneralDataRepository.getActiveComplaintsInMaterialMasterGeneralData(materialDBIDs);
            for (MaterialMasterGeneralDatas materialMasterGeneralData : materialMasterGeneralDataList) {
                MaterialMasterPlantDataResponse response = new MaterialMasterPlantDataResponse();
                response.setMaterialCode(materialMasterGeneralData.getMaterialCode());
                response.setRecordNo(masterPlantDataRequestMap.get(materialMasterGeneralData.getMaterialCode()).getRecordNo());
                if (!complaintMaterialMasterGeneralDataDbList.contains(materialMasterGeneralData.getId())) {
                    recordsToBeDeleted.add(materialMasterDataDB.get(materialMasterGeneralData.getMaterialCode()));
                    response.setMessage(messageHelper.getMessage(MessageKeys.MATERIAL_MASTER_PLANT_DATA_SUCCESSFULLY_DELETED));
                    response.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                } else {
                    response.setMessage(messageHelper.getMessage(MessageKeys.MATERIAL_MASTER_PLANT_DATA_ASSOCIATION_TO_COMPLAINT));
                    response.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                }
                materialMasterPlantDataResponseList.add(response);
            }
            if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                materialMasterPlantDataRepository.deleteMaterialMasterPlantDataList(recordsToBeDeleted);
                logger.info("Records Deleted ", recordsToBeDeleted.size());
            }
        }
        return materialMasterPlantDataResponseList;
    }

    @Override
    public MaterialMasterPlantDatas fetchMaterialMasterPlantDatas(String material, String plant) {
     Result materialPlantDataResult = materialMasterPlantDataRepository.fetchMaterialMasterPlantDatas(material, plant);
        return materialPlantDataResult.first().isPresent() ? materialPlantDataResult.listOf(MaterialMasterPlantDatas.class)
        .get(0) : null;
    }
}