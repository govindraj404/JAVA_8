package com.sap.ic.cmh.masterdata.materialmastergeneraldata.service;

import java.util.List;
import java.util.function.Function;

import com.sap.cds.ql.StructuredType;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataRequest;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataResponse;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;

public interface MaterialMasterGeneralDataService {

    List<MaterialMasterGeneralDataResponse> deleteMaterialMasterGeneralDataList(List<MaterialMasterGeneralDataRequest> materialMasterGeneralDataRequest);

    public <E extends StructuredType<E>> MaterialMasterGeneralDatas fetchMaterialMasterGeneralData(String materialMasterGeneralData, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

    public MaterialMasterGeneralDatas fetchMaterialMasterGeneralDataBasedOnCode(String materialCode);
}
