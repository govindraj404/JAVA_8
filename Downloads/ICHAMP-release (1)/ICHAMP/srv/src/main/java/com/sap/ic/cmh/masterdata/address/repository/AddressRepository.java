package com.sap.ic.cmh.masterdata.address.repository;

import java.util.List;
import java.util.Map;
import com.sap.cds.Result;
import java.util.function.Function;
import com.sap.cds.ql.StructuredType;

public interface AddressRepository {
    Map<String, String> getAddressMap(List<String> addresses);

    Map<String, String> getCompanyCodeInAddress(List<String> keySet);

    Map<String, String> getPlantDetails(List<String> addressIdList);

    Map<String, String> getBusinessPartner(List<String> addressIdList);

    void deleteAddressList(List<String> addressIdList);

    public Result getAddressDetailsBasedOnAddress(String address);

    public Result getAddressDetails(String id);

    public <E extends StructuredType<E>> Result fetchAddress(String addressId, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);
}