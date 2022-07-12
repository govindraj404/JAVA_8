package com.sap.ic.cmh.masterdata.purchaseorganization.service;

import com.sap.cds.Result;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationRequest;
import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationResponse;
import com.sap.ic.cmh.masterdata.purchaseorganization.repository.PurchaseOrganizationRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import cds.gen.masterdataservice.PurchaseOrganizations;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

@Component
public class PurchaseOrganizationServiceImpl implements PurchaseOrganizationService {

    public static final Logger logger = LoggerHelper.getLogger(PurchaseOrganizationServiceImpl.class);

    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired
    private PurchaseOrganizationRepository purchaseOrganizationRepository;

    @Override
    public List<PurchaseOrganizationResponse> deletePurchaseOrganizationList(List<PurchaseOrganizationRequest> purchaseOrganizationRequestList) {
        LoggerHelper.logMethodEntry(logger, "PurchaseOrganizationServiceImpl", "deletePurchaseOrganizationList");
        List<PurchaseOrganizationResponse> purchaseOrganizationResponseList = new ArrayList<>();
        AtomicInteger integerAtomic = new AtomicInteger();
        Map<String, String> purchaseOrganizationWithRecordNoMap = new HashMap<>();
        purchaseOrganizationRequestList.forEach(purchaseOrganizationRequest ->
                purchaseOrganizationWithRecordNoMap.put(purchaseOrganizationRequest.getPurchaseOrganization(), String.valueOf(integerAtomic.incrementAndGet())));
        List<String> purchaseOrganizationList = new ArrayList<>(purchaseOrganizationWithRecordNoMap.keySet());
        final Map<String, String> purchaseOrganizationWithIdMap = purchaseOrganizationRepository.getPurchaseOrganizationMap(purchaseOrganizationList);
        purchaseOrganizationWithRecordNoMap.keySet().forEach(purchaseOrganization -> {
            if (!purchaseOrganizationWithIdMap.containsValue(purchaseOrganization)) {
                logger.info("Purchase organization Not Found in DB", purchaseOrganization);
                PurchaseOrganizationResponse purchaseOrganizationResponse = new PurchaseOrganizationResponse();
                String recordNumber = purchaseOrganizationWithRecordNoMap.get(purchaseOrganization);
                purchaseOrganizationResponse.setPurchaseOrganization(purchaseOrganization);
                purchaseOrganizationResponse.setMessage(messageHelper.getMessage(MessageKeys.PURCHASE_ORGANIZATION_DOES_NOT_EXIST));
                purchaseOrganizationResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                purchaseOrganizationResponse.setRecordNo(recordNumber);
                purchaseOrganizationResponseList.add(purchaseOrganizationResponse);
            }
        });
        if (!CollectionUtils.isEmpty(purchaseOrganizationWithIdMap)) {
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> purchaseOrganizationsId = new ArrayList<>(purchaseOrganizationWithIdMap.keySet());
            List<String> complaintPurchaseOrganizationDbList = purchaseOrganizationRepository.getActiveComplaintsInPurchaseOrganization(purchaseOrganizationsId);
            for (String purchaseOrganizationId : purchaseOrganizationsId) {
                PurchaseOrganizationResponse purchaseOrganizationResponse = new PurchaseOrganizationResponse();
                String purchaseOrganization = purchaseOrganizationWithIdMap.get(purchaseOrganizationId);
                String recordNumber = purchaseOrganizationWithRecordNoMap.get(purchaseOrganization);
                purchaseOrganizationResponse.setRecordNo(recordNumber);
                purchaseOrganizationResponse.setPurchaseOrganization(purchaseOrganization);
                if (!complaintPurchaseOrganizationDbList.contains(purchaseOrganizationId)) {
                    recordsToBeDeleted.add(purchaseOrganizationId);
                    purchaseOrganizationResponse.setMessage(messageHelper.getMessage(MessageKeys.PURCHASE_ORGANIZATION_SUCCESSFULLY_DELETED));
                    purchaseOrganizationResponse.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                } else {
                    purchaseOrganizationResponse.setMessage(messageHelper.getMessage(MessageKeys.PURCHASE_ORGANIZATION_ASSOCIATION_TO_COMPLAINT));
                    purchaseOrganizationResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                }
                purchaseOrganizationResponseList.add(purchaseOrganizationResponse);
            }
            if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                purchaseOrganizationRepository.deletePurchaseOrganizationList(recordsToBeDeleted);
                logger.info("Records Deleted ", recordsToBeDeleted.size());
            }
        }
        return purchaseOrganizationResponseList;
    }

    @Override
    public PurchaseOrganizations fetchPurchaseOrganization(String purchaseOrganizations) {
      Result purchaseOrgResult =  purchaseOrganizationRepository.fetchPurchaseOrganization(purchaseOrganizations);
        return purchaseOrgResult.first().isPresent() ? purchaseOrgResult.listOf(PurchaseOrganizations.class).get(0)
        : null;
    }
}
