package com.sap.ic.cmh.masterdata.reason.service;


import cds.gen.masterdataservice.Reasons;
import cds.gen.masterdataservice.Reasons_;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.plant.repository.PlantRepository;
import com.sap.ic.cmh.masterdata.reason.repository.RejectReasonRepository;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationRequest;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationResponse;
import com.sap.ic.cmh.masterdata.storagelocation.repository.StorageLocationRepository;
import com.sap.ic.cmh.masterdata.storagelocation.service.StorageLocationServiceImpl;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class RejectReasonServiceImplTest {
    @InjectMocks
    RejectReasonServiceImpl rejectReasonServiceImpl;

    @Mock
    LocaleMessageHelper messageHelper;
    @Mock
    Result result;
    @Mock
    private RejectReasonRepository rejectReasonRepository;


    private Row row;
    private Optional<Row> opt;


    private Reasons reasons;

    private List<Reasons> reasonsLists = new ArrayList<>();

    public Map<String,String> reasonsMap = new HashMap<>();


    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);
        reasons = Struct.create(Reasons.class);
        reasons.setCode("code");
        reasons.setDescription("Description");

    }

    @Test
    public void testFetchSubItemTypesNull() {
        when(rejectReasonRepository.fetchReasonBasedOnCode(reasons.getCode())).thenReturn(result);
        rejectReasonServiceImpl.fetchReasonBasedOnCode(reasons.getCode());
    }

}
