package com.sap.ic.cmh.customercomplaint.handler;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftCancelEventContext;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.attachment.controller.AttachmentController;
import com.sap.ic.cmh.customercomplaint.service.AttachmentOperationsService;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.customercomplaintservice.Attachments;
import cds.gen.customercomplaintservice.CustomerComplaints;
import cds.gen.customercomplaintservice.CustomerComplaints_;
import cds.gen.customercomplaintservice.SingleItems;

@Component
@ServiceName("CustomerComplaintService")
public class CustomerComplaintHandler implements EventHandler {

	@Autowired
	Messages messages;
    @Autowired
    AttachmentOperationsService attachmentOperationsService;
    @Autowired
    ObjectStoreUtil objectStoreUtil;
    @Autowired
    AttachmentController attachmentController;
    @Autowired
    CqnAnalyzerUtil cqnAnalyzerUtil;


	public static final Logger logger = LoggerHelper.getLogger(CustomerComplaintHandler.class);
	private static final String COMPLAINT_HANDLER = "CustomerComplaintHandler";

	/**
	 * Set the initial status of complaint
	 *
	 * @param complaint
	 */
	@Before(event = DraftService.EVENT_DRAFT_NEW)
	public void beforeComplaintDraftCreation(DraftNewEventContext context, CustomerComplaints customerComplaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER,
				"beforeComplaintDraftCreation");
		if (customerComplaint.getIndividualComplaint() != null
				&& Boolean.TRUE.equals(customerComplaint.getIndividualComplaint())) {
			customerComplaint.setItem(Struct.create(SingleItems.class));
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER,
				"beforeComplaintDraftCreation");
	}
    
    /**
     * Method to handle attachments that got deleted in draft mode
     * @param customerComplaints
     */
    @On(event=CdsService.EVENT_UPDATE, entity =CustomerComplaints_.CDS_NAME)
    public void onCustomerComplaintsUpdate(CustomerComplaints customerComplaints){
        LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER,"onCustomerComplaintsUpdate");
        logger.info("Inside update method of complaints");
        logger.info("complaintId ::: {} ",customerComplaints.getId());
        /* Selecting from Draft attachment table based on IsActiveEntity and HasDraftEntity as false
        for both create/update */
        List<Attachments> attachmentListFromDraftDB = attachmentOperationsService.getDraftAttachmentDetailsHasActiveEntity(customerComplaints.getId());
        logger.info("attachmentListFromDraftDB size:: {} ",attachmentListFromDraftDB.size());
        List<Attachments> attachmentListFromDB = attachmentOperationsService.getAttachmentDetailsBasedOnComplaintId(customerComplaints.getId());
        logger.info("attachmentListFromDB size:: {} ",attachmentListFromDB.size());
        if(!CollectionUtils.isEmpty(attachmentListFromDraftDB) && !CollectionUtils.isEmpty(attachmentListFromDB)){
            logger.info("attachmentListFromDraftDB and attachmentListFromDB not empty");
          List<Attachments> diffList = attachmentListFromDB.stream().filter(activeDBRecord-> !attachmentListFromDraftDB.contains(activeDBRecord))
            .collect(Collectors.toList());
            logger.info("diffList ::: {} ",diffList);
            if(!CollectionUtils.isEmpty(diffList)){
                logger.info("diffList not empty ");
                logger.info("diffList size:: {} ",diffList.size());
                for(Attachments attachmentDifference : diffList){
                    logger.info("Before deleting attachments");
                    attachmentController.deleteAttachment(attachmentDifference.getId());
                  }
            }
             
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER,"onCustomerComplaintsUpdate");
    }
    
	/**
	 * Method to handle deleting of attachments when complaint is discarded.
	 * @param context
	 */
    @On(event=DraftService.EVENT_DRAFT_CANCEL, entity =CustomerComplaints_.CDS_NAME)
    public void onCustomerComplaintsDelete(DraftCancelEventContext context){
        LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER,"onCustomerComplaintsDelete");
        logger.info("Inside Delete method");
        Map<String, Object> keys = cqnAnalyzerUtil.provideTargetKeysDraft(context);
		String complaintId = (String) keys.get(CustomerComplaints.ID);
        logger.info("complaintId ::: {} ",complaintId);
        /* Selecting from Draft attachment table based on IsActiveEntity and HasDraftEntity as false
        for both create/update */
        List<Attachments> attachmentListFromDraftDB = attachmentOperationsService.getDraftAttachmentDetailsBasedOnComplaintId(complaintId);
        logger.info("attachmentListFromDraftDB size:: {} ",attachmentListFromDraftDB.size());
        if(!CollectionUtils.isEmpty(attachmentListFromDraftDB)){
            logger.info("attachmentListFromDraftDB not empty");
              for(Attachments attachmentFromDraftDB : attachmentListFromDraftDB){
                logger.info("Before deleting attachments");
                attachmentController.deleteAttachment(attachmentFromDraftDB.getId());
              }
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER,"onCustomerComplaintsDelete");
    }
    
}
