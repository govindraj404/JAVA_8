package com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas_;


@Component
public class MaterialMasterGeneralDataRepositoryImpl implements MaterialMasterGeneralDataRepository {
    public static final Logger logger = LoggerHelper.getLogger(MaterialMasterGeneralDataRepositoryImpl.class);

    @Autowired
    private PersistenceService db;

    @Override
    public Map<String, String> getMaterialMasterGeneralDataMap(List<String> materialMasterGeneralDatas) {

        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getMaterialMasterGeneralDataMap");
        CqnSelect materialMasterGeneralDataSelect = Select.from(MaterialMasterGeneralDatas_.class).columns("ID", "materialCode")
                .where(b -> b.materialCode().in(materialMasterGeneralDatas));
        List<MaterialMasterGeneralDatas> materialMasterGeneralDataList = db.run(materialMasterGeneralDataSelect).listOf(MaterialMasterGeneralDatas.class);
        logger.info("Material MasterGeneral Data :", materialMasterGeneralDataList.size());
        return materialMasterGeneralDataList.stream().collect(Collectors.toMap(MaterialMasterGeneralDatas::getId, MaterialMasterGeneralDatas::getMaterialCode));
    }

    @Override
    public List<String> getActiveComplaintsInMaterialMasterGeneralData(List<String> materialMasterGeneralDatasId) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getActiveComplaintsInMaterialMasterGeneralData");
        List<String> complaintsList = new ArrayList<>();
        CqnSelect complaintsSelect = Select.from(Complaints_.class).where(b -> b.material_ID().in(materialMasterGeneralDatasId).and(b.complaintStatus_code()
                .ne(Constants.COMPLAINT_CLOSED)));
        final List<Complaints> complaints = db.run(complaintsSelect).listOf(Complaints.class);
        complaintsList.addAll(complaints.stream().map(Complaints::getMaterialId).collect(Collectors.toList()));
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getActiveComplaintsInMaterialMasterGeneralData");
        return complaintsList;
    }

    @Override
    public void deleteMaterialMasterGeneralDataList(List<String> recordsToBeDeleted) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteInactiveMaterialMasterGeneralData");
        CqnDelete delete = Delete.from(MaterialMasterGeneralDatas_.class).where(cc -> cc.ID().in(recordsToBeDeleted));
        long deleteCount = db.run(delete).rowCount();
        logger.info("Records Deleted count: ", deleteCount);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deleteInactiveMaterialMasterGeneralData");
    }
    
    /**
     * This method is used to get MaterialMasterGeneralData details
     *
     * @param materialMasterGeneralData
     * @return MaterialMasterGeneralData
     */
    @Override
    public <E extends StructuredType<E>> Result fetchMaterialMasterGeneralData(
            String materialMasterGeneralData, String message, Class<E> targetClass,
            Function<E, Object> targetClassAttribute) {
                LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "fetchMaterialMasterGeneralData");
                Result aMaterialMasterGeneralData = null;
                if (!ObjectUtils.isEmpty(materialMasterGeneralData)) {
                     aMaterialMasterGeneralData = (db.run(Select.from(MaterialMasterGeneralDatas_.class)
                            .where(b -> b.materialCode().eq(materialMasterGeneralData))));
                }
                LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "fetchMaterialMasterGeneralData");
                return aMaterialMasterGeneralData;
    }
    
    /**
     * This method is used to get MaterialMasterGeneralData details based on Material Code
     * @param materialCode
     * @return
     */
    @Override
    public Result fetchMaterialMasterGeneralDataBasedOnCode(String materialCode) {
        return db.run(Select.from(MaterialMasterGeneralDatas_.class)
        .where(b -> b.materialCode().eq(materialCode)));
    }

    /**
     * This method is used to get MaterialMasterGeneralData based on Id
     * @param id
     */
	@Override
	public Result getMaterialMasterGeneralDataBasedOnId(String id) {
		return db.run(Select.from(MaterialMasterGeneralDatas_.class).columns(b->b.ID()).where(b -> b.ID().eq(id)));
	}
}