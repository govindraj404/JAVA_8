package com.sap.ic.cmh.configuration.handler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.messaging.TopicMessageEventContext;
import com.sap.cds.services.runtime.RequestContextRunner;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.MessageService;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.returnpo.model.ReturnOrderBackendStatus;
import com.sap.ic.cmh.returnpo.model.ReturnPurchaseOrderRequestPayload;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.tenancy.model.Subscription;
import com.sap.ic.cmh.tenancy.service.TenantSubscriptionService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.managequalitynotificationservice.QualityNotifications;


@Component
public class MessageServiceHandler implements EventHandler {

    @Autowired
    MessageService messageService;
    @Autowired
    BusinessObjectConfigurationDao businessObjectConfigurationDao;
    @Autowired
    DestinationService destinationService;
    @Autowired
    ReturnPurchaseOrderService returnPurchaseOrderService;
    @Autowired
    DestinationConfigurationDao destinationConfigDao;
    @Autowired
    CommonFunctions commonFunctions;
    @Autowired
    HttpService httpservice;
    @Autowired
    TenantSubscriptionService tenantSubscriptionService;

    private static final Logger logger = LoggerFactory.getLogger(MessageServiceHandler.class);
    private static final String MESSAGE_SERVICE_HANDLER = "MessageServiceHandler";

    @On(service = "cmh-messaging", event = "qnbocreate")
    public synchronized void onAutomaticQNCreate(TopicMessageEventContext topicContext) {
        LoggerHelper.logMethodEntry(logger, MESSAGE_SERVICE_HANDLER, "onAutomaticQNCreate");
        logger.info("[MessageServiceHandler][onAutomaticQNCreate] started");
        getModifiedUser(topicContext).run(req -> {
        	logger.info("Executing for QN Create");
            executeQualityNotificationCreateUpdate(topicContext);
        });
        logger.info("[MessageServiceHandler][onAutomaticQNCreate] end");
        LoggerHelper.logMethodExit(logger, MESSAGE_SERVICE_HANDLER, "onAutomaticQNCreate");
    }

    @On(service = "cmh-messaging", event = "qnboupdate")
    public synchronized void onAutomaticQNUpdate(TopicMessageEventContext topicContext) {
        LoggerHelper.logMethodEntry(logger, MESSAGE_SERVICE_HANDLER, "onAutomaticUpdate");
        logger.info("[MessageServiceHandler][onAutomaticUpdate] started");
        getModifiedUser(topicContext).run(req -> {
        	logger.info("Executing for QN Update");
        	executeQualityNotificationCreateUpdate(topicContext);
        });
        logger.info("[MessageServiceHandler][onAutomaticUpdate] end");
        LoggerHelper.logMethodExit(logger, MESSAGE_SERVICE_HANDLER, "onAutomaticUpdate");
    }

    /**
     * Claim BO update
     * 
     * @param topicContext
     */
    @On(service = "cmh-messaging", event = "claimboupdate")
    public void onAutomaticClaimStatusUpdate(TopicMessageEventContext topicContext) {
        LoggerHelper.logMethodEntry(logger, MESSAGE_SERVICE_HANDLER, "onAutomaticClaimStatusUpdate");
        getModifiedUser(topicContext).run(req -> {
            final ObjectMapper mapper = new ObjectMapper();
            Map<String, Object> claimMap = new HashMap<>();
            
            TypeReference<HashMap<String, Object>> typeRef = new TypeReference<HashMap<String, Object>>() {
            };
            Map<String, Object> claimMessageMap = new HashMap<>();
            try {
                String tenentId = topicContext.getUserInfo().getTenant();
                logger.info("[MessageServiceHandler][onAutomaticClaimStatusUpdate] tenentId={}", tenentId);
                String subdomain = getSubdomain(topicContext);
                String msgId = topicContext.getMessageId();
                String payload = topicContext.getData();
                logger.info(
                        "[MessageServiceHandler][onAutomaticClaimStatusUpdate] info message id is={}, payload is ={}",
                        msgId, payload);
                claimMap = mapper.readValue(payload, typeRef);
                Map<String, Object> eventMessagePayloadMap = (Map<String, Object>) claimMap.get("data");
                logger.info("Map1: " + eventMessagePayloadMap);

                if (!CollectionUtils.isEmpty(eventMessagePayloadMap)) {
                    List<Object> claimMessagePayloadList = (List<Object>) eventMessagePayloadMap.get("PNWTYH");
                    logger.info("claimMessagePayloadList: " + claimMessagePayloadList);
                   claimMessageMap = (Map<String, Object>) claimMessagePayloadList.get(0);
                    logger.info("claimMessageMap: {}", claimMessageMap);
                    String claimNumber = (String) claimMessageMap.get("CLAIM_NUMBER");
                    String claimStatus = (String) claimMessageMap.get("CLAIM_STATUS");
                    logger.info("[MessageServiceHandler][onAutomaticClaimStatusUpdate] claimNumber: {}", claimNumber);
                    logger.info("[MessageServiceHandler][onAutomaticClaimStatusUpdate] claimStatus: {}", claimStatus);
                    String claimId = messageService.checkIfBOExists(claimNumber, Constants.CLAIM_CODE);
                    if (StringUtils.isNotBlank(claimId) && StringUtils.isNotBlank(claimStatus)) {
                        httpservice.callCpiFlowForautoCreation(Constants.UPDATE_CLAIM_STATUS, claimMessageMap, "",subdomain);
                    }
                }

            } catch (JsonProcessingException e) {
                LoggerHelper.logExceptionWithMessage(logger,
						"[MessageServiceHandler][onAutomaticClaimStatusUpdate] Exception launched while trying to parse String to Map.",
						e);
                return;
            } catch (Exception e) {
            	 LoggerHelper.logExceptionWithMessage(logger,
 						"[MessageServiceHandler][onAutomaticClaimStatusUpdate] Exception launched while trying to parse String to Map.{}",
 						e);
            	 logger.info("Destination is not found for Claim status Update");
             messageService.formPayloadAndUpdateOdata(claimMessageMap,Constants.CLAIM_CODE);
            }
        });
        LoggerHelper.logMethodExit(logger, MESSAGE_SERVICE_HANDLER, "onAutomaticClaimStatusUpdate");
    }


    @On(service = "cmh-messaging", event = "rpoboupdate")
    public void onAutomaticReturnOrderStatusUpdate(TopicMessageEventContext topicContext) {
        LoggerHelper.logMethodEntry(logger, MESSAGE_SERVICE_HANDLER, "onAutomaticReturnOrderStatusUpdate");
       
        getModifiedUser(topicContext).run(req -> {
            ReturnPurchaseOrderRequestPayload rpoMessagePayload = convertEventMessagePayloadToReturnPurchaseOrder(
                    topicContext);
            String subdomain = getSubdomain(topicContext);
            Map<String, Object> returnPORequestMap = new HashMap<>();
            try {
                logger.info("[MessageServiceHandler][onrpoupdate] rpoMessagePayload: {}", rpoMessagePayload);
                if (null != rpoMessagePayload) {
                    logger.info("[MessageServiceHandler][onrpoupdate] rpoMessagePayload not null: {}",
                            rpoMessagePayload);
                    returnPORequestMap = commonFunctions.convertObjectToMap(rpoMessagePayload);
                    logger.info("[MessageServiceHandler][onrpoupdate] returnPORequestMap: {}", returnPORequestMap);
                    logger.info("[MessageServiceHandler][onrpoupdate] calling CPI for rpo");
                    httpservice.callCpiFlowForautoCreation(Constants.UPDATE_RPO_STATUS, returnPORequestMap, "",subdomain);
                }
            } catch (Exception e) {
            	LoggerHelper.logExceptionWithMessage(logger,
 						"[MessageServiceHandler][onAutomaticReturnOrderStatusUpdate] Exception while executing onAutomaticReturnOrderStatusUpdate {}",
 						e);
            	logger.info("Destination is not found for Return Purchase Order Status Update");
            	messageService.formPayloadAndUpdateOdata(returnPORequestMap, Constants.RETURNPO_CODE);
            }
        });
        LoggerHelper.logMethodExit(logger, MESSAGE_SERVICE_HANDLER, "onAutomaticReturnOrderStatusUpdate");
    }

    private ReturnPurchaseOrderRequestPayload convertEventMessagePayloadToReturnPurchaseOrder(
            TopicMessageEventContext topicContext) {
        ReturnPurchaseOrderRequestPayload rpoMessagePayload = null;
        final ObjectMapper mapper = new ObjectMapper();
        Map<String, Object> rpoPayloadMap = new HashMap<>();
        TypeReference<HashMap<String, Object>> typeRef = new TypeReference<HashMap<String, Object>>() {
        };
        try {
            String payload = topicContext.getData();
            logger.info("[MessageServiceHandler][convertEventMessagePayloadToReturnPurchaseOrder] tenentId : {} ",
                    topicContext.getUserInfo().getTenant());
            rpoPayloadMap = mapper.readValue(payload, typeRef);
            Map<String, Object> eventMessagePayloadMap = (Map<String, Object>) rpoPayloadMap.get("data");
            logger.info("rpo eventMessagePayloadMap size: ", eventMessagePayloadMap.size());

            if (!CollectionUtils.isEmpty(eventMessagePayloadMap)) {
                if (eventMessagePayloadMap.containsKey("RPO_ID")) {
                    rpoMessagePayload = getRPOdata(eventMessagePayloadMap);
                } else if (eventMessagePayloadMap.containsKey("EKKO")) {
                    rpoMessagePayload = getROPdataForCompletion(eventMessagePayloadMap);
                }
            }

        } catch (ODataException e) {
            logger.error(
                    "[MessageServiceHandler][convertEventMessagePayloadToReturnPurchaseOrder] Exception converting EventMessagePayload to ReturnPurchaseOrder Payload ");
        } catch (JsonProcessingException e) {
            logger.error(
                    "[MessageServiceHandler][convertEventMessagePayloadToReturnPurchaseOrder] Exception launched while trying to parse String to Map.");
        }
        return rpoMessagePayload;
    }

    private ReturnPurchaseOrderRequestPayload getRPOdata(Map<String, Object> eventMessagePayloadMap) {
        ReturnPurchaseOrderRequestPayload rpoMessagePayload = null;
        ReturnOrderBackendStatus backendStatus = new ReturnOrderBackendStatus();
        List<ReturnOrderBackendStatus> rpoBackendStatusList = new ArrayList<>();
        String rpoData = (String) eventMessagePayloadMap.get("RPO_ID");
        logger.info("[getRPOdata] rpo data: " + rpoData);
        if (StringUtils.isNotBlank(rpoData) && rpoData.contains("/")) {
            String[] spl = rpoData.split("/");
            if (null != spl && spl.length == 3) {
                rpoMessagePayload = new ReturnPurchaseOrderRequestPayload();
                rpoMessagePayload.setRpoNumber(spl[0]);
                rpoMessagePayload.setPlantNumber(spl[1]);
                backendStatus.setStatusCode(spl[2]);
                rpoBackendStatusList.add(backendStatus);
                rpoMessagePayload.setRpoBackendStatusList(rpoBackendStatusList);
                logger.info("rpoMessagePayload: {} " , rpoMessagePayload);
            }
        } else {
            logger.info("rpo data is not correct. / is not available");
        }
        return rpoMessagePayload;
    }

    private ReturnPurchaseOrderRequestPayload getROPdataForCompletion(Map<String, Object> eventMessagePayloadMap) {
        ReturnPurchaseOrderRequestPayload rpoMessagePayload = null;
        List<Object> rpoList = (List<Object>) eventMessagePayloadMap.get("EKKO");
        logger.info("rpoList: " + rpoList);
        Map<String, Object> rpoMap = (Map<String, Object>) rpoList.get(0);
        logger.info("rpoMap: " + rpoMap);
        if (!CollectionUtils.isEmpty(rpoMap)) {
            rpoMessagePayload = new ReturnPurchaseOrderRequestPayload();
            rpoMessagePayload.setRpoNumber((String) rpoMap.get("EBELN"));
            List<Object> rpoStatusList = (List<Object>) rpoMap.get("MSR_D_HEAD");
            logger.info("rpoStatusList: " + rpoStatusList);
            Map<String, Object> rpoStatusMap = (Map<String, Object>) rpoStatusList.get(0);
            if (!CollectionUtils.isEmpty(rpoStatusMap)) {
                setRPOStatusForCompletion(rpoStatusMap, rpoMessagePayload);
            }
        }
        return rpoMessagePayload;
    }

    private void setRPOStatusForCompletion(Map<String, Object> rpoStatusMap,
            ReturnPurchaseOrderRequestPayload rpoMessagePayload) {
    	ReturnOrderBackendStatus backendStatus = new ReturnOrderBackendStatus();
        List<ReturnOrderBackendStatus> rpoBackendStatusList = new ArrayList<>();
        String rpoCompleteStatus = (String) rpoStatusMap.get("COMPLETE");
        if ("X".equalsIgnoreCase(rpoCompleteStatus)) {
        	String returnOrderCompletedStatus = "RPC";
            backendStatus.setStatusCode(returnOrderCompletedStatus);
            rpoBackendStatusList.add(backendStatus);
            rpoMessagePayload.setRpoBackendStatusList(rpoBackendStatusList);
            List<Object> rpoPlantList = (List<Object>) rpoStatusMap.get("MSR_D_EXECUTED");
            logger.info("rpoPlantList: " + rpoPlantList);
            Map<String, Object> rpoPlantMap = (Map<String, Object>) rpoPlantList.get(0);
            if (!CollectionUtils.isEmpty(rpoPlantMap)) {
                setRPOPlantForCompletion(rpoPlantMap, rpoMessagePayload);
            }
        }
    }

    private void setRPOPlantForCompletion(Map<String, Object> rpoPlantMap,
            ReturnPurchaseOrderRequestPayload rpoMessagePayload) {
        rpoMessagePayload.setPlantNumber((String) rpoPlantMap.get("WERKS"));
    }

    private RequestContextRunner getModifiedUser(TopicMessageEventContext topicContext) {
        final List<String> roles = Arrays.asList("Complaint.Create", "Complaint.Read", "Complaint.Update",
                "Claim.Create", "Claim.Read", "Claim.Update", "Stream.Read", "Stream.Update",
                "QualityNotification.Create", "QualityNotification.Read", "QualityNotification.Update",
                "ReturnPurchaseOrder.Create", "ReturnPurchaseOrder.Read", "ReturnPurchaseOrder.Update",
                "CostCollector.Create", "CostCollector.Read", "CostCollector.Update", "CostCollector.Delete",
                "ConditionType.Read", "ServiceMaterial.Read", "DestinationConfiguration.Read",
                "BusinessObjectConfiguration.Read", "ClaimStatusMapping.Read");
        return topicContext.getCdsRuntime().requestContext().modifyUser(user -> 
            user.setRoles(new HashSet<>(roles)));
    }

    private String getSubdomain(TopicMessageEventContext topicContext) {

        String tenantId = topicContext.getUserInfo().getTenant();
        String subdomain = "";
        logger.info("tenant id={}", tenantId);
        List<Subscription> subscriptionList = tenantSubscriptionService.getSaasSubscription().getSubscriptions();

        for (Subscription subscription : subscriptionList) {
            if (subscription.getConsumerTenantId().equalsIgnoreCase(tenantId)) {
                logger.info("Subdomain tenant found");
                subdomain = subscription.getSubdomain();
                break;
            }
        }
        return subdomain;
    }

    private void setPersonResponsibleFromPayload(List<Object> ihpaList, Map<String, Object> qmelMap) {
        boolean isPersonResponsible=false;
        List<Object>newIhpaList=new ArrayList<>();
        String configuredDestination = qmelMap.get("MAWERK").toString();
        Result result = businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(
                Constants.COMPLAINT_TYPE, configuredDestination, Constants.QUALITYNOTIFICATION_CODE,
                QualityNotifications.PERSON_RESPONSIBLE_ROLE);
        if (result.first().isPresent()) {
            logger.info(
                    "[MessageServiceHandler][setPersonResponsibleFromPayload] Person Responsible Role Configuration Present");
            String configuredPersonResponsibleRole = result.list().get(0)
                    .get(BusinessObjectConfigurations.BUSINESS_OBJECT_VALUE).toString().toLowerCase();

            for (Object ihpa : ihpaList) {
                Map<String, Object> ihpaMap = (Map<String, Object>) ihpa;
                if (ihpaMap.get("PARVW").toString().equalsIgnoreCase(configuredPersonResponsibleRole)) {
                    newIhpaList.add(ihpa);
                    qmelMap.put("IHPA", newIhpaList);
                    isPersonResponsible=true;
                    logger.info(
                            "[MessageServiceHandler][setPersonResponsibleFromPayload] Matching Person Responsible Role Found");
                    break;
                }
            }
            if(!isPersonResponsible){
                qmelMap.put("IHPA", newIhpaList);
            }
        }
        
    }
    
    /**
     * Execute Create or Update for Quality Notification
     * @param topicContext
     */
	public void executeQualityNotificationCreateUpdate(TopicMessageEventContext topicContext) {
		final ObjectMapper mapper = new ObjectMapper();
		Map<String, Object> qualityNotificationPayloadMap = new HashMap<>();
		TypeReference<HashMap<String, Object>> typeRef = new TypeReference<HashMap<String, Object>>() {
		};
		Map<String, Object> qmelMap = new HashMap<>();
		try {
		String payload = topicContext.getData();
		String subdomain = getSubdomain(topicContext);
		qualityNotificationPayloadMap = mapper.readValue(payload, typeRef);
		Map<String, Object> eventMessagePayloadMap = (Map<String, Object>) qualityNotificationPayloadMap
		        .get("data");
		if (!CollectionUtils.isEmpty(eventMessagePayloadMap)) {
		    List<Object> qmelList = (List<Object>) eventMessagePayloadMap.get("QMEL");
		    logger.info("qmelList:{} ", qmelList);
		    qmelMap = (Map<String, Object>) qmelList.get(0);
		    logger.info("qmelMap:{} ", qmelMap);
		    List<Object> qmfeList = (List<Object>) qmelMap.get("QMFE");
		    Map<String, Object> qmfeMap = (Map<String, Object>) qmfeList.get(0);
		    logger.info("qmfeMap:{} ", qmfeMap);
		    List<Object> jestList = (List<Object>) qmfeMap.get("JEST");
		    Map<String, Object> jestMap = (Map<String, Object>) jestList.get(0);
		    logger.info("jestMap:{} ", jestMap);
		    List<Object> ihpaList = (List<Object>) jestMap.get("IHPA");
		    setPersonResponsibleFromPayload(ihpaList, qmelMap);
		        httpservice.callCpiFlowForautoCreation(Constants.CREATE_UPDATE_QN, qmelMap, "", subdomain);
		}
         } catch (Exception e) {
		LoggerHelper.logExceptionWithMessage(logger,
				"[MessageServiceHandler][onAutomaticQNCreate] Exception launched while trying to parse String to Map.{} ",
				e);
		 logger.info("Destination not found for QN Create/Update");
		messageService.formPayloadAndUpdateOdata(qmelMap, Constants.QUALITYNOTIFICATION_CODE);
		
         }
	}

}
