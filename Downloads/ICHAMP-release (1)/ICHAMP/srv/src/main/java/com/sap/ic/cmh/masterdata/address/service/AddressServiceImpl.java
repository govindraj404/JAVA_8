package com.sap.ic.cmh.masterdata.address.service;


import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import com.sap.ic.cmh.masterdata.address.model.AddressRequest;
import com.sap.ic.cmh.masterdata.address.model.AddressResponse;
import com.sap.ic.cmh.masterdata.address.repository.AddressRepository;
import cds.gen.masterdataservice.Addresses;
import com.sap.cds.Result;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.services.messages.Messages;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

@Service
public class AddressServiceImpl implements AddressService {
    public static final Logger logger = LoggerHelper.getLogger(AddressServiceImpl.class);

    @Autowired
    private AddressRepository addressRepository;
    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired 
    Messages messages;



    /**
     * Process the address data for deletion
     *
     * @param addressList
     * @return list of Address
     */
    @Override
    public List<AddressResponse> deleteAddressList(List<AddressRequest> addressList) {
        LoggerHelper.logMethodEntry(logger, "AddressServiceImpl", "deleteAddressList");
        List<AddressResponse> addressResponseList = new ArrayList<>();
        AtomicInteger integerAtomic = new AtomicInteger();
        Map<String, String> addressesWithRecordNoMap = new HashMap<>();
        addressList.forEach(address -> addressesWithRecordNoMap.put(address.getAddress(), String.valueOf(integerAtomic.incrementAndGet())));
        List<String> addressKeys = new ArrayList<>(addressesWithRecordNoMap.keySet());
        Map<String, String> addressDb = addressRepository.getAddressMap(addressKeys);
        addressList.forEach(address -> {
            if (!addressDb.containsKey(address.getAddress())) {
                AddressResponse addressResponse = new AddressResponse();
                addressResponse.setAddress(address.getAddress());
                addressResponse.setMessage(messageHelper.getMessage(MessageKeys.ADDRESS_DOES_NOT_EXIST));
                addressResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                addressResponse.setRecordNo(addressesWithRecordNoMap.get(address.getAddress()));
                addressResponseList.add(addressResponse);
            }
        });
        if (!addressDb.isEmpty()) {
            List<String> addressIdList = new ArrayList<>(addressDb.keySet());
            Map<String, String> companyCodeWithAddressMap = addressRepository.getCompanyCodeInAddress(addressIdList);
            Map<String, String> plantWithAddressMap = addressRepository.getPlantDetails(addressIdList);
            Map<String, String> businessPartnerWithAddressMap = addressRepository.getBusinessPartner(addressIdList);
            List<String> recordsToBeDeleted = new ArrayList<>();
            addressDb.keySet().forEach(addressId -> {
                AddressResponse addressResponse = new AddressResponse();
                addressResponse.setAddress(addressId);
                addressResponse.setRecordNo(addressesWithRecordNoMap.get(addressId));
                if (!companyCodeWithAddressMap.containsKey(addressId) && !plantWithAddressMap.containsKey(addressId) &&
                        !businessPartnerWithAddressMap.containsKey(addressId)) {
                    recordsToBeDeleted.add(addressId);
                    addressResponse.setMessage(messageHelper.getMessage(MessageKeys.ADDRESS_SUCCESSFULLY_DELETED));
                    addressResponse.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                } else {
                    addressResponse.setMessage(messageHelper.getMessage(MessageKeys.ADDRESS_ASSOCIATION_TO_RECORDS));
                    addressResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                }
                addressResponseList.add(addressResponse);
            });
            if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                addressRepository.deleteAddressList(recordsToBeDeleted);
                logger.info("Addresses deleted for : ", recordsToBeDeleted.toString());
            }
        }
        LoggerHelper.logMethodExit(logger, "AddressServiceImpl", "deleteAddressList");
        return addressResponseList;
    }


    @Override
    public Addresses getAddress(String id){
        Result addressResult = addressRepository.getAddressDetails(id);
		return addressResult.first().isPresent() ? addressResult.listOf(Addresses.class).get(0)
				: null;
    }


    @Override
    public Addresses getAddressDetailsBasedOnAddress(String address) {
        Result addressResult = addressRepository.getAddressDetailsBasedOnAddress(address);
        return addressResult.first().isPresent() ? addressResult.listOf(Addresses.class).get(0)
				: null;
    }


    @Override
    public <E extends StructuredType<E>> Addresses fetchAddress(String addressId, String message,
            Class<E> targetClass, Function<E, Object> targetClassAttribute) {
                Addresses address = null;
                Result addressResult = addressRepository.fetchAddress(addressId, message, targetClass, targetClassAttribute);
                if (addressResult != null && addressResult.first(Addresses.class).isPresent()) {
                    address = addressResult.first(Addresses.class).get();
                } else {
                    messages.error(message).target("in", targetClass, targetClassAttribute);
                }       
        return address;
    } 
}