package com.sap.ic.cmh.masterdata.address.service;

import cds.gen.masterdataservice.Addresses;
import com.sap.cds.ql.StructuredType;
import com.sap.ic.cmh.masterdata.address.model.AddressRequest;
import com.sap.ic.cmh.masterdata.address.model.AddressResponse;
import java.util.List;
import java.util.function.Function;

public interface AddressService {

    List<AddressResponse> deleteAddressList(List<AddressRequest> addressList);

    public Addresses getAddress(String id);

    public Addresses getAddressDetailsBasedOnAddress(String address);

    public <E extends StructuredType<E>> Addresses fetchAddress(String addressId, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

}