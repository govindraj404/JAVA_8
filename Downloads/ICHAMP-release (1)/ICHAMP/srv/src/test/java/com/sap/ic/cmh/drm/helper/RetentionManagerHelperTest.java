package com.sap.ic.cmh.drm.helper;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.text.ParseException;

public class RetentionManagerHelperTest {

    @InjectMocks
    RetentionManagerHelper helper;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void residencyCheckTest(){
        String dataFrom="2021/07/09";
        String dateTo="2020/20/16";
        helper.residencyCheck(dataFrom,dateTo);
    }

    @Test
    public void residencyCheckNullTest(){
        String dataFrom="0000/00/00";
        String dateTo="0000/00/00";
        helper.residencyCheck(dataFrom,dateTo);
    }

    @Test
    public void residencyCheckElseTest(){
        String dataFrom="2021-07-09";
        String dateTo="2020-20-16";
        helper.residencyCheck(dataFrom,dateTo);
    }

    @Test
    public void residencyCheckElse2Test(){
        String dataFrom="0000-00-00";
        String dateTo="0000-00-00";
        helper.residencyCheck(dataFrom,dateTo);
    }

    @Test
    public void residencyCheckExpTest(){
        String dataFrom="test";
        String dateTo="Test";
        helper.residencyCheck(dataFrom,dateTo);
    }
    @Test
    public void residencyCheckNull1Test(){
        String dataFrom="0000/00/00";
        String dateTo="0000/00/00";
        helper.residencyCheck(dataFrom,dateTo);
    }

    @Test
    public void residencyCheckElse3Test(){
        String dataFrom="20210709";
        String dateTo="20202016";
        helper.residencyCheck(dataFrom,dateTo);
    }

    @Test
    public void residencyCheckElse4Test(){
        String dataFrom="00000000";
        String dateTo="00000000";
        helper.residencyCheck(dataFrom,dateTo);
    }

    @Test
    public void isDeleteOrBlockTest() throws ParseException {
        String dataFrom="00000000";
          helper.isDeleteOrBlock(dataFrom);
    }
    @Test
    public void isDeleteOrBlockElseTest() throws ParseException {
        String dataFrom="2023-20-02";
        helper.isDeleteOrBlock(dataFrom);
    }

    @Test
    public void convertLocalDateTest() throws ParseException {
        String dataFrom="2023-20-02";
        helper.convertLocalDate(dataFrom);
    }
}
