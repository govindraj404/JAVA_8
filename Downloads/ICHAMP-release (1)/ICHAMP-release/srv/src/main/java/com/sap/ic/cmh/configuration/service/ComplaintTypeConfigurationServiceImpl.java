package com.sap.ic.cmh.configuration.service;


import cds.gen.configurationservice.ComplaintTypeConfigurations;
import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeConfigurationDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ComplaintTypeConfigurationServiceImpl implements ComplaintTypeConfigurationService {

    public static final Logger logger = LoggerHelper.getLogger(ComplaintTypeConfigurationServiceImpl.class);
    private static final String COMPLAINT_TYPE_CONFIGURATION_SERVICE_IMPL = "ComplaintTypeConfigurationServiceImpl";
    @Autowired
    ComplaintTypeConfigurationDao complaintTypeConfigDao;

    /**
     * Get complaint type configuration identifier
     */
    @Override
    public Result getComplaintTypeConfiguration() {
        return complaintTypeConfigDao.getComplaintTypeConfiguration();
    }

    /**
     * Get Complaint Type based on ID
     */
    @Override
    public ComplaintTypeConfigurations getAllComplaintTypesDetails(String id) {
        Result configurationResult = complaintTypeConfigDao.getAllComplaintTypesDetails(id);
        return configurationResult.first().isPresent() ? configurationResult.listOf(ComplaintTypeConfigurations.class).get(0)
                : null;
    }

    /**
     * fetch item complaint type is active flag  based on Id
     */
    public boolean getActive(String id) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIGURATION_SERVICE_IMPL, "getActive");
        Result compType = complaintTypeConfigDao.getAllComplaintTypesDetails(id);
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIGURATION_SERVICE_IMPL, "getActive");
        if (compType.first().isPresent()) {
            return compType.listOf(ComplaintTypeConfigurations.class).get(0).getIsActive();
        }
        return false;
    }
}
