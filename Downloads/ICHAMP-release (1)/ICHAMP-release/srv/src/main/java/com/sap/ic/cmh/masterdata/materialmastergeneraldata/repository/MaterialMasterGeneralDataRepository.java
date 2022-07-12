package com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import com.sap.cds.Result;
import com.sap.cds.ql.StructuredType;

public interface MaterialMasterGeneralDataRepository {
    Map<String, String> getMaterialMasterGeneralDataMap(List<String> materialMasterGeneralDatas);

    List<String> getActiveComplaintsInMaterialMasterGeneralData(List<String> materialMasterGeneralDatasId);

    void deleteMaterialMasterGeneralDataList(List<String> recordsToBeDeleted);

    public <E extends StructuredType<E>> Result fetchMaterialMasterGeneralData(String materialMasterGeneralData, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

    public Result fetchMaterialMasterGeneralDataBasedOnCode(String materialCode);
    
    public Result getMaterialMasterGeneralDataBasedOnId(String id);
}