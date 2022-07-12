package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintReasons;
import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonsDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ComplaintReasonsServiceImpl implements ComplaintReasonsService {

    public static final Logger logger = LoggerHelper.getLogger(ComplaintReasonsServiceImpl.class);
    private static final String COMPLAINT_REASON_SERVICE_IMPL = "ComplaintReasonsServiceImpl";

    @Autowired
    ComplaintReasonsDao complaintReasonsDao;

    /**
     * Fetch Complaint Reasons
     *
     * @public
     */
    @Override
    public Result getAllComplaintReasonsOrderByIdentifier() {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_SERVICE_IMPL, "getAllComplaintReasonsOrderByIdentifier");
        return complaintReasonsDao.getAllComplaintReasonsOrderByIdentifier();
    }
    /**
     * Fetch Complaint Reasons based on Id
     *@param {@link String} id
     * @public
     */
    @Override
    public ComplaintReasons getComplaintReasonBasedOnID(String reasonId) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_SERVICE_IMPL, "getAllComplaintReasonsOrderByIdentifier");
        Result complaintReasonsResult=complaintReasonsDao.getComplaintReasonDetailsBasedOnID(reasonId);
        return complaintReasonsResult.first().isPresent() ? complaintReasonsResult.listOf(ComplaintReasons.class).get(0)
                : null;

    }
    @Override
    public boolean getActive(String id ){
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_SERVICE_IMPL, "getActive");
        Result complaintReasonsResult = complaintReasonsDao.getComplaintReasonDetailsBasedOnID(id);
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_SERVICE_IMPL, "getActive");

        if(complaintReasonsResult.first().isPresent()){
            return  complaintReasonsResult.listOf(ComplaintReasons.class).get(0).getIsActive();
        }

        return  false;
    }
}