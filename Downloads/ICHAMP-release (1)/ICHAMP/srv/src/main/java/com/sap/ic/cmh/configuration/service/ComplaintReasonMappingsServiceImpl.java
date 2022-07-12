package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintReasonMappings;
import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonMappingsDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ComplaintReasonMappingsServiceImpl implements ComplaintReasonMappingsService {
    @Autowired
    ComplaintReasonMappingsDao complaintReasonMappingsDao;

    private static final String COMPLAINT_REASON_MAPPINGS_SERVICE_IMPL = "ComplaintReasonMappingsServiceImpl";
    private static final Logger logger = LoggerFactory.getLogger(ComplaintReasonMappingsServiceImpl.class);

    /**
     * Get Complaint Reason Map identifier.
     *
     */
    @Override
    public Result getComplaintReasonMapIdentifier() {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_SERVICE_IMPL, "getComplaintReasonMapIdentifier");
        return complaintReasonMappingsDao.getComplaintReasonMapOrderByIdentifier();
    }

    /**
     * Get complaintreason mapping details based on ID
     */
    @Override
    public ComplaintReasonMappings getComplaintReasonMappingsDetails(String id){
        Result complaintReasonMappingsResult = complaintReasonMappingsDao.getComplaintReasonMapDetailsBasedOnId(id);
        return complaintReasonMappingsResult.first().isPresent() ? complaintReasonMappingsResult.listOf(ComplaintReasonMappings.class).get(0)
                : null;
    }
}
