package com.sap.ic.cmh.masterdata.defectcode.repository;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.DefectCodes_;

@Component
public class DefectCodeRepositoryImpl implements DefectCodeRepository {

	@Autowired
	private PersistenceService db;

	public static final Logger logger = LoggerHelper.getLogger(DefectCodeRepositoryImpl.class);

	@Override
	public Result fetchDefectCode(String defectCode, String defectGroup) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "fetchDefectCode");
		Result defectCodeResult = null;
		if (!ObjectUtils.isEmpty(defectCode)) {
			defectCodeResult = (db.run(Select.from(DefectCodes_.class)
					.where(b -> b.code().eq(defectCode).and(b.defectGroup_code().eq(defectGroup)))));
		}
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "fetchDefectCode");
		return defectCodeResult;
	}

	@Override
	public Result fetchDefectCodeByDefectGroup(String defectGroup) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "fetchDefectCodeByDefectGroup");
		Result defectCodeResult = db
				.run(Select.from(DefectCodes_.class).where(b -> b.defectGroup_code().eq(defectGroup)));
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "fetchDefectCodeByDefectGroup");
		return defectCodeResult;
	}

}
