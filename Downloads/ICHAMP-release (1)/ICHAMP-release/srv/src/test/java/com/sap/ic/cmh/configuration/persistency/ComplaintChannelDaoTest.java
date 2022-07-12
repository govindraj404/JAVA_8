package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ComplaintChannels;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ComplaintChannelDaoTest {
    @InjectMocks
    ComplaintChannelDao complaintChannelDao;
    @Mock
    PersistenceService db;
    @Mock
    Result result;
    @Mock
    Runnable run;

    @Mock
    PersistenceService mockDb;

    ComplaintChannels complaintChannels;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintChannels = Struct.create(ComplaintChannels.class);
        complaintChannels.setCode("channelcode");
        complaintChannels.setIdentifier(77);
    }

    @Test
    public void getAllComplaintChannelsCodeTest(){
        Optional<ComplaintChannels> opt = Optional.of(complaintChannels);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        complaintChannelDao.getAllComplaintChannelsCodeAndIDByCode(complaintChannels.getCode());
    }

    @Test
    public void getAllComplaintChannelsTest(){
        Optional<ComplaintChannels> opt = Optional.of(complaintChannels);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        complaintChannelDao.getAllComplaintChannelsOrderByIdentifier();
    }

    @Test
    public void testGetComplaintChannelDetailsBasedOnId() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintChannelDao.getComplaintChannelDetailsBasedOnId(any(String.class));
    }
}
