package com.sap.ic.cmh.complaint.handler;

import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.complaint.validation.ComplaintValidation;
import com.sap.ic.cmh.complaint.validation.StreamValidation;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.BusinessObjects_;
import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Streams;
import cds.gen.complaintservice.Streams_;
import cds.gen.masterdataservice.ActionPreconditions;
import cds.gen.masterdataservice.ActionPreconditions_;
import cds.gen.masterdataservice.Actions;
import cds.gen.masterdataservice.Actions_;

@Component
@ServiceName("ComplaintService")
public class StreamHandler implements EventHandler {

     @Autowired
     ComplaintValidation complaintValidation;

     @Autowired
     StreamValidation streamValidation;

     @Autowired
     ComplaintService complaintService;
     @Autowired
     Messages messages;

     @Autowired
     PersistenceService db;

     @Autowired
     MasterDataService masterDataService;

     @Autowired
     StreamService streamService;

     public static final Logger logger = LoggerHelper.getLogger(StreamHandler.class);
     private static final String STREAM_HANDLER = "StreamHandler";

     /**
      * Validation for Stream during Adapt Stream
      *
      * @param streams
      */
     @Before(event = CdsService.EVENT_UPDATE, entity = Streams_.CDS_NAME)
     public void beforeStreamsUpdate(java.util.stream.Stream<Streams> streams) {
          LoggerHelper.logMethodEntry(logger, STREAM_HANDLER, "beforeStreamsUpdate");
          streams.forEach(eachStream -> {
               streamValidation.validateStreamEdit(eachStream);
               messages.throwIfError();
          });
          LoggerHelper.logMethodExit(logger, STREAM_HANDLER, "beforeStreamsUpdate");
     }

     /**
      * Validate if relevant streams are closed and close the complaint accordingly.
      * 
      * @param streams
      */
     @After(event = CdsService.EVENT_UPDATE, entity = Streams_.CDS_NAME)
     public void afterStreamsUpdate(Streams streams) {
          LoggerHelper.logMethodEntry(logger, STREAM_HANDLER, "afterStreamsUpdate");
          Streams stream = streamService.getStream(streams.getId()).single(Streams.class);
          streamService.updateComplaintStatusBasedOnAllStreamStatus(stream.getParentIDId());
          LoggerHelper.logMethodExit(logger, STREAM_HANDLER, "afterStreamsUpdate");
     }

     /**
      * Validation for Business Object during Adapt Stream
      *
      * @param eachBo
      */
     @Before(event = DraftService.EVENT_DRAFT_PATCH, entity = BusinessObjects_.CDS_NAME)
     public void beforeStreamBusinessObjectsPatch(BusinessObjects eachBo) {
          LoggerHelper.logMethodEntry(logger, STREAM_HANDLER, "beforeStreamBusinessObjectsPatch");
          streamValidation.validateBusinessObjectsEdit(eachBo);
          LoggerHelper.logMethodExit(logger, STREAM_HANDLER, "beforeStreamBusinessObjectsPatch");
     }

     /**
      * Validation for Stream during Adapt Stream
      *
      * @param streams
      */
     @Before(event = DraftService.EVENT_DRAFT_PATCH, entity = Streams_.CDS_NAME)
     public void beforeStreamPatch(Streams streams) {
          LoggerHelper.logMethodEntry(logger, STREAM_HANDLER, "beforeStreamPatch");
          streamValidation.validateStreamEdit(streams);
          LoggerHelper.logMethodExit(logger, STREAM_HANDLER, "beforeStreamPatch");
     }

     /**
      * Validation for Stream during Adapt Stream
      *
      * @param bObjects
      */
     @Before(event = CdsService.EVENT_UPDATE, entity = BusinessObjects_.CDS_NAME)
     public void beforeBusinessObjectsUpdate(java.util.stream.Stream<BusinessObjects> bObjects) {
          LoggerHelper.logMethodEntry(logger, STREAM_HANDLER, "beforeBusinessObjectsUpdate");
          bObjects.forEach(eachBo -> {
               streamValidation.validateBusinessObjectsEdit(eachBo);
               messages.throwIfError();
          });
          LoggerHelper.logMethodExit(logger, STREAM_HANDLER, "beforeBusinessObjectsUpdate");
     }

     /**
      * Validate Business Objects, enable/disable action buttons based on action pre
      * conditions and authorization roles
      * 
      * @param businessObjectsStream
      */
     @After(event = { CdsService.EVENT_READ }, entity = BusinessObjects_.CDS_NAME)
     public void afterBusinessObjectsRead(CdsReadEventContext context, Stream<BusinessObjects> businessObjectsStream) {
          LoggerHelper.logMethodEntry(logger, STREAM_HANDLER, "afterBusinessObjectsRead");
          UserInfo user = context.getUserInfo();
          List<ActionPreconditions> actionPreConditionList = db.run(Select.from(ActionPreconditions_.class))
                    .listOf(ActionPreconditions.class);
          List<Actions> actionList = db.run(Select.from(Actions_.class)).listOf(Actions.class);
          businessObjectsStream.forEach(businessObject -> {
               businessObject.setIsValidActionPrecondition(false);

               Result businessObjectTemp = streamService.getBusinessObject(businessObject.getId());
               BusinessObjects fetchedBusinessObject = businessObjectTemp.list().get(0).as(BusinessObjects.class);
               String complaintId = fetchedBusinessObject.get(BusinessObjects.COMPLAINT).toString();
               String businessObjectType = fetchedBusinessObject.getBusinessObjectTypeCode();

               if (complaintId != null && businessObjectType != null || businessObjectType.isEmpty()) {
                    Complaints complaint = complaintService.getComplaintDetails(complaintId);
                    Boolean isRelevant = fetchedBusinessObject.getIsRelevant() && (complaint != null
                              && !complaint.getComplaintStatusCode().equals(Constants.COMPLAINT_DISCARDED));
                    switch (businessObjectType) {
                         case Constants.QUALITYNOTIFICATION_CODE:
                              businessObject.setIsVisible(user.hasRole("QualityNotification.Create"));
                              businessObject.setIsValidActionPrecondition(businessObject.getIsVisible());
                              break;
                         case Constants.CLAIM_CODE:
                              businessObject.setIsVisible(user.hasRole("Claim.Create"));
                              businessObject
                                        .setIsValidActionPrecondition(businessObject.getIsVisible() && isActionEnabled(
                                                  actionList, actionPreConditionList, businessObjectType, complaintId));
                              break;
                         case Constants.RETURNPO_CODE:
                              businessObject.setIsVisible(user.hasRole("ReturnPurchaseOrder.Create"));
                              businessObject
                                        .setIsValidActionPrecondition(businessObject.getIsVisible() && isActionEnabled(
                                                  actionList, actionPreConditionList, businessObjectType, complaintId));
                              break;
                         case Constants.SUPPLIER_EIGHTD_CODE:
                              businessObject.setIsVisible(user.hasRole("SupplierIssueProcess.Create"));
                              businessObject
                                        .setIsValidActionPrecondition(businessObject.getIsVisible() && isActionEnabled(
                                                  actionList, actionPreConditionList, businessObjectType, complaintId));
                              break;
                         default:
                              break;
                    }
                    businessObject.setIsVisible(
                              isRelevant && businessObject.getIsVisible());
                    setButtonEnablement(businessObject);
               }
               if (businessObject.getBusinessObjectID() != null
                         && businessObject.getBusinessObjectID().getIdentifier() != null) {
                    businessObject.getBusinessObjectID().setIdentifier(
                              businessObject.getBusinessObjectID().getIdentifier().replaceFirst("^0+(?!$)", ""));
               }
          });
          LoggerHelper.logMethodExit(logger, STREAM_HANDLER, "afterBusinessObjectsRead");
     }

     private void setButtonEnablement(BusinessObjects businessObject) {
          if (businessObject.getIsActiveEntity() != null) {
               businessObject
                         .setIsValidActionPrecondition(
                                   businessObject.getIsValidActionPrecondition() && businessObject.getIsActiveEntity());
          }
     }

     /**
      * Check if action can be enabled based on action and action pre-conditions
      * 
      * @param actionList
      * @param actionPreConditionList
      * @param businessObjectType
      * @param complaintId
      * @return
      */
     boolean isActionEnabled(List<Actions> actionList, List<ActionPreconditions> actionPreConditionList,
               String businessObjectType, String complaintId) {
          LoggerHelper.logMethodEntry(logger, STREAM_HANDLER, "isActionEnabled");
          String action = null;
          String targetBusinessObject = null;
          String targetBusinessObjectStatus = null;

          for (Actions actions : actionList) {
               if (Objects.equals(actions.getBusinessObjectTypeCode(), businessObjectType)) {
                    action = actions.getCode();
               }
          }

          for (ActionPreconditions actionPreConditions : actionPreConditionList) {
               if (Objects.equals(actionPreConditions.getCodeCode(), action)) {
                    targetBusinessObject = actionPreConditions.getBusinessObjectTypeCode();
                    targetBusinessObjectStatus = actionPreConditions.getBusinessObjectStatusCode();
               }
          }

          Result businessObjectsWithStatus = streamService.getBusinessObjectsWithStatus(complaintId,
                    targetBusinessObject, targetBusinessObjectStatus);
          LoggerHelper.logMethodExit(logger, STREAM_HANDLER, "isActionEnabled");
          return businessObjectsWithStatus.first().isPresent();
     }
}
