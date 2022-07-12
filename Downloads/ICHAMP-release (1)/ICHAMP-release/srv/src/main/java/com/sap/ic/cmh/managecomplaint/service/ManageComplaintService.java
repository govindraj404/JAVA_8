package com.sap.ic.cmh.managecomplaint.service;

import java.math.BigDecimal;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.qualitynotification.model.QualityNotificationDetails;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.managecomplaintservice.Complaints;
import cds.gen.managecomplaintservice.Complaints_;
import cds.gen.managequalitynotificationservice.QualityNotifications;

@Service
public class ManageComplaintService {

    @Autowired
    @Qualifier("ManageComplaintService")
    private CdsService cdsService;
    @Autowired
    ComplaintService complaintService;
    @Autowired
    ConfigurationService configService;
    @Autowired
    Messages messages;
    @Autowired
    CommonFunctions commonFunctions;
    @Autowired
    ComplaintsDao complaintDao;

    private static final Logger logger = LoggerFactory.getLogger(ManageComplaintService.class);
    private static final String MANAGE_COMPLAINT_SERVICE = "ManageComplaintService";

    public Complaints createComplaintAutomatic(QualityNotifications manageQualityNotifications) {
        LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_SERVICE, "createComplaint");
        Complaints complaint = Struct.create(Complaints.class);

        logger.info("[create complaint][ComplaintServiceImpl] message.getMaterial {}",
                manageQualityNotifications.getMaterialCode());
        logger.info("[create complaint][ComplaintServiceImpl] message.getSupplier {}",
                manageQualityNotifications.getSupplierCode());
        logger.info("[create complaint][ComplaintServiceImpl] message.getPlant {}",
                manageQualityNotifications.getPlantCode());
        logger.info("[create complaint][ComplaintServiceImpl] message.getPurchasingOrg {}",
                manageQualityNotifications.getPurchaseOrganisationCode());

        complaint.setMaterialCode(manageQualityNotifications.getMaterialCode());
        complaint.setPlantCode(manageQualityNotifications.getPlantCode());
        complaint.setSupplierCode(manageQualityNotifications.getSupplierCode());
        complaint.setPurchaseOrganisationCode(
                manageQualityNotifications.getPurchaseOrganisationCode());

        if (StringUtils.isNotBlank(String.valueOf(manageQualityNotifications.getQuantity()))
                && manageQualityNotifications.getQuantity().intValue() > 0) {
            complaint.setQuantity(manageQualityNotifications.getQuantity());
        }
        complaint.setUnitCode(manageQualityNotifications.getUnit());

        complaint.setCreationType(Constants.COMPLAINT_AUTOMATIC);
        complaint.setReferenceNumber(manageQualityNotifications.getReferenceNumber());
        CqnInsert insert = Insert.into(Complaints_.class).entry(complaint);
        logger.info(
                "[create complaint][ComplaintServiceImpl] before cdsService call for create Complaint");

        return cdsService.run(insert).listOf(Complaints.class).get(0);
    }

    /**
     * Update Master data attributes for complaint when QN details are updated in the target system
     */

    public void updateMasterDataForComplaint(String complaintId,
            QualityNotificationDetails qualityNotificationDetails, BigDecimal quantity,
            String unit) {
        logger.info(
                "[create complaint][updateMasterDataForComplaint] qualityNotificationDetails: {}",
                qualityNotificationDetails);

        cds.gen.complaintservice.Complaints draftComplaints =
                complaintService.getComplaintDetails(complaintId);
        Complaints manageComplaints =
                commonFunctions.convertComplaintToManageComplaint(draftComplaints);

        if (null != qualityNotificationDetails.getMaterial()) {
            manageComplaints.setMaterialCode(qualityNotificationDetails.getMaterial());
        }
        manageComplaints.setPlantCode(qualityNotificationDetails.getPlant());
        manageComplaints.setSupplierCode(qualityNotificationDetails.getSupplier());
        if (null != qualityNotificationDetails.getPurchasingOrg()) {
            manageComplaints
                    .setPurchaseOrganisationCode(qualityNotificationDetails.getPurchasingOrg());
        }

        if (quantity != null && quantity.compareTo(BigDecimal.ZERO) > 0) {
            manageComplaints.setQuantity(quantity);
        }
        manageComplaints.setUnitCode(unit);
        manageComplaints.setIsEventSpecificRequest(true);
        manageComplaints.setReferenceNumber(qualityNotificationDetails.getReferenceNumber());
        CqnUpdate update = Update.entity(Complaints_.class).data(manageComplaints);
        cdsService.run(update).listOf(Complaints.class).get(0);

    }

    /**
     * Update Master data attributes for complaint when QN details are updated in the target system
     */

    public void updateMasterDataForComplaints(QualityNotifications manageQualityNotifications) {
        logger.info(
                "[create complaint][updateMasterDataForComplaint] qualityNotificationDetails: {}",
                manageQualityNotifications);

        cds.gen.complaintservice.Complaints draftComplaints =
                complaintService.getComplaintDetails(manageQualityNotifications.getComplaintId());
        Complaints manageComplaints =
                commonFunctions.convertComplaintToManageComplaint(draftComplaints);

        if (null != manageQualityNotifications.getMaterialCode()) {
            manageComplaints.setMaterialCode(manageQualityNotifications.getMaterialCode());
        }
        if(null != manageQualityNotifications.getPlantCode()){
            manageComplaints.setPlantCode(manageQualityNotifications.getPlantCode());
        }
       if(null != manageQualityNotifications.getSupplierCode()){
        manageComplaints.setSupplierCode(manageQualityNotifications.getSupplierCode());
       }
       
        if (null != manageQualityNotifications.getPurchaseOrganisationCode()) {
            manageComplaints
                    .setPurchaseOrganisationCode(manageQualityNotifications.getPurchaseOrganisationCode());
        }

        manageComplaints.setQuantity(manageQualityNotifications.getQuantity());
        manageComplaints.setUnitCode(manageQualityNotifications.getUnit());
        manageComplaints.setIsEventSpecificRequest(true);
        manageComplaints.setReferenceNumber(manageQualityNotifications.getReferenceNumber());
        CqnUpdate update = Update.entity(Complaints_.class).data(manageComplaints);
        cdsService.run(update).listOf(Complaints.class).get(0);
    }

}
