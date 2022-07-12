package com.sap.ic.cmh.masterdata.address.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;


public class AddressResponse extends CommonResponse {

    @JsonProperty("Address")
    private String address;

    @Override
    public String toString() {
        return "AddressResponse{" +
                "status='" + getStatus() + '\'' +
                ", message='" + getMessage() + '\'' +
                ", address='" + address + '\'' +
                '}';
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

}