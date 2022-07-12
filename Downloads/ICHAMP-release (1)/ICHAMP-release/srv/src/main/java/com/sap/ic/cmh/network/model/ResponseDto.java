package com.sap.ic.cmh.network.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ResponseDto {
    String content;
    int status;
}
