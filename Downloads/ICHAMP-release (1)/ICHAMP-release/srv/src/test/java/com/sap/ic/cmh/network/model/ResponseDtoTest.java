package com.sap.ic.cmh.network.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class ResponseDtoTest {
    @InjectMocks
    ResponseDto dto=new ResponseDto("test",9);

    @Before
    public void beforeClass(){
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setMethodTest(){
        dto.setContent("test");
        dto.setStatus(1);
        dto.getContent();
        dto.getStatus();
    }
}
