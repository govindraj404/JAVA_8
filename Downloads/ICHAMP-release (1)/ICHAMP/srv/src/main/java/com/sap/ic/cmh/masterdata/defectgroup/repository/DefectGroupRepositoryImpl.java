package com.sap.ic.cmh.masterdata.defectgroup.repository;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.DefectGroups_;

@Component
public class DefectGroupRepositoryImpl implements DefectGroupRepository{

    @Autowired
	private PersistenceService db;

	public static final Logger logger = LoggerHelper.getLogger(DefectGroupRepositoryImpl.class);
    
    /**
	 * This method is used to get Defect code details
	 *
	 * @param code
	 * @return DefectCodes
	 */
    @Override
    public Result fetchDefectGroupCode(String defectGroupCode) {
        LoggerHelper.logMethodEntry(logger, "DefectGroupRepositoryImpl", "fetchDefectGroupCode");
		Result defectGroupResult = null;
		if (!ObjectUtils.isEmpty(defectGroupCode)) {
			defectGroupResult = db.run(Select.from(DefectGroups_.class).where(b -> b.code().eq(defectGroupCode)));
		}
		LoggerHelper.logMethodExit(logger, "DefectGroupRepositoryImpl", "fetchDefectGroupCode");
		return defectGroupResult;
    }
    
}
