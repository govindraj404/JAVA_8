package com.sap.ic.cmh.configuration.service;
import cds.gen.configurationservice.TargetTypes;

import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.TargetTypeConfigurationDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


@Service
public class TargetTypeConfigurationServiceImpl implements TargetTypeConfigurationService {

	@Autowired
	TargetTypeConfigurationDao targetTypeConfigDao;

	private static final Logger logger = LoggerFactory.getLogger(TargetTypeConfigurationServiceImpl.class);
	private static final String TARGET_TYPE_CONFIGURATION_SERVICE_IMPL = "TargetTypeConfigurationServiceImpl";

	/**
	 * Get target type configuration identifier
	 */
	@Override
	public Result getTargetTypeConfigurations() {
		return targetTypeConfigDao.getTargetTypeConfigurations();
	}

	/**
	 * Get target type configuration based on id
	 */
	@Override
	public Result getTargetTypeConfigurationCodeBasedOnId(String targetTypeConfigurationId) {
		return targetTypeConfigDao.getTargetTypeConfigurationCodeBasedOnId(targetTypeConfigurationId);
	}

	/**
	 * Get ts type configuration based on code
	 */
	@Override
	public Result getTargetTypeConfigurationIdBasedOnCode(String targetTypeConfigurationCode) {
		return targetTypeConfigDao.getTargetTypeConfigurationIdBasedOnCode(targetTypeConfigurationCode);
	}

	/**
	 * Get TargetType details based on ID
	 *
	 *   @public
	 */
	@Override
	public TargetTypes getTargetTypesDetails(String id) {
		LoggerHelper.logMethodEntry(logger, TARGET_TYPE_CONFIGURATION_SERVICE_IMPL, "getTargetTypesDetails");
		Result targetTypesResult = targetTypeConfigDao.getTargetTypeConfigurationCodeBasedOnId1(id);
		return targetTypesResult.first().isPresent() ? targetTypesResult.listOf(TargetTypes.class).get(0)
				: null;
	}

	/**
	 * fetch Target Type is active flag  based on Id
	 */
	@Override
	public boolean getActive(String id ){
		LoggerHelper.logMethodEntry(logger, TARGET_TYPE_CONFIGURATION_SERVICE_IMPL, "getActive");
		Result targetTypeResult = targetTypeConfigDao.getTargetTypeConfigurationCodeBasedOnId1(id);
		LoggerHelper.logMethodExit(logger, TARGET_TYPE_CONFIGURATION_SERVICE_IMPL, "getActive");
		if(targetTypeResult.first().isPresent()){
			return  targetTypeResult.listOf(TargetTypes.class).get(0).getIsActive();
		}
		return  false;
	}

}
