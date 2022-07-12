package com.sap.ic.cmh.masterdata.companycode.service;

import com.sap.cds.Result;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeRequest;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeResponse;
import com.sap.ic.cmh.masterdata.companycode.repository.CompanyCodeRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import cds.gen.masterdataservice.CompanyCodes;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

@Service
public class CompanyCodeServiceImpl implements CompanyCodeService {
    public static final Logger logger = LoggerHelper.getLogger(CompanyCodeServiceImpl.class);
    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired
    private CompanyCodeRepository companyCodeRepository;
    @Autowired
    Messages messages;

    @Override
    public List<CompanyCodeResponse> deleteCompanyCodeList(
            List<CompanyCodeRequest> companyCodeLists) {
        LoggerHelper.logMethodEntry(logger, "CompanyCodeServiceImpl", "deleteCompanyCodeList");
        List<CompanyCodeResponse> companyCodeResponseList = new ArrayList<>();
        AtomicInteger integerAtomic = new AtomicInteger();
        Map<String, String> companyCodesRequestMap = new LinkedHashMap<>();
        companyCodeLists.forEach(companyCodeList -> companyCodesRequestMap.put(
                companyCodeList.getCompanyCode(), String.valueOf(integerAtomic.incrementAndGet())));
        List<String> companyCodeList = new ArrayList<>(companyCodesRequestMap.keySet());
        Map<String, String> companyCodeMapFromDB =
                companyCodeRepository.getCompanyCodeMap(companyCodeList);
        companyCodesRequestMap.keySet().forEach(companyCode -> {
            if (!companyCodeMapFromDB.containsValue(companyCode)) {
                CompanyCodeResponse companyCodeResponse = new CompanyCodeResponse();
                companyCodeResponse.setCompanyCode(companyCode);
                companyCodeResponse.setMessage(
                        messageHelper.getMessage(MessageKeys.COMPANY_CODE_DOES_NOT_EXIST));
                companyCodeResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                companyCodeResponse.setRecordNo(companyCodesRequestMap.get(companyCode));
                companyCodeResponseList.add(companyCodeResponse);
            }
        });
        // Check for active complaint records
        if (!CollectionUtils.isEmpty(companyCodeMapFromDB)) {
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> companyCodeIdList = new ArrayList<>(companyCodeMapFromDB.keySet());
            List<String> complaintCompanyCodeDbList =
                    companyCodeRepository.getActiveComplaintsInCompanyCode(companyCodeIdList);
            companyCodeIdList.forEach(companyCodeId -> {
                CompanyCodeResponse companyCodeResponse = new CompanyCodeResponse();
                companyCodeResponse.setCompanyCode(companyCodeMapFromDB.get(companyCodeId));
                companyCodeResponse.setRecordNo(
                        companyCodesRequestMap.get(companyCodeResponse.getCompanyCode()));
                if (!complaintCompanyCodeDbList.contains(companyCodeId)) {
                    // Collect records to be deleted
                    recordsToBeDeleted.add(companyCodeId);
                    companyCodeResponse.setMessage(messageHelper
                            .getMessage(MessageKeys.COMPANY_CODE_SUCCESSFULLY_DELETED));
                    companyCodeResponse.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                } else {
                    // Collect the error records to be sent to the response
                    companyCodeResponse.setMessage(messageHelper
                            .getMessage(MessageKeys.COMPANY_CODE_ASSOCIATION_TO_COMPLAINT));
                    companyCodeResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                }
                companyCodeResponseList.add(companyCodeResponse);
            });
            if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                companyCodeRepository.deleteCompanyCodeList(recordsToBeDeleted);
                logger.info("CompanyCodes deleted for : ", recordsToBeDeleted.toString());
            }
        }
        LoggerHelper.logMethodExit(logger, "CompanyCodeServiceImpl", "deleteCompanyCodeList");
        return companyCodeResponseList;
    }

    @Override
    public <E extends StructuredType<E>> CompanyCodes fetchCompanyCode(String companyCode,
            String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        CompanyCodes companyCodes = null;
        Result fetchCompanyCodeResult = companyCodeRepository.fetchCompanyCode(companyCode, message,
                targetClass, targetClassAttribute);
        if (fetchCompanyCodeResult != null
                && fetchCompanyCodeResult.first(CompanyCodes.class).isPresent()) {
            companyCodes = fetchCompanyCodeResult.first(CompanyCodes.class).get();
        } else {
            messages.error(message).target("in", targetClass, targetClassAttribute);
        }
        return companyCodes;
    }

    @Override
    public CompanyCodes fetchCompanyCodesBasedOnCode(String companyCode) {
        Result companyCodResult = companyCodeRepository.fetchCompanyCodesBasedOnCode(companyCode);
        return companyCodResult.first().isPresent()
                ? companyCodResult.listOf(CompanyCodes.class).get(0)
                : null;
    }
}
