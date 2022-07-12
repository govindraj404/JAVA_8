package com.sap.ic.cmh.metering.service;

import cds.gen.com.sap.ic.cmh.meteringlog.MeteringLogs;
import cds.gen.complaintservice.Complaints;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.metering.client.MeteringClient;
import com.sap.ic.cmh.tenancy.model.SaasSubscription;
import com.sap.ic.cmh.tenancy.model.Subscription;
import com.sap.ic.cmh.tenancy.service.TenantSubscriptionService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class MeteringServiceImplTest {
    @InjectMocks
    MeteringServiceImpl impl;
    @Mock
    MeteringClient meteringClient;
    @Mock
    private PersistenceService db;
    @Mock
    private Result result;
    @Mock
    TenantSubscriptionService tenantSubscriptionService;
    @Mock
    SaasSubscription subscription;
    @Mock
    UserInfo userInfo;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test(expected = Exception.class)
    public void sendElse() {
        MeteringLogs logs= Struct.create(MeteringLogs.class);
        Optional<MeteringLogs> emptyOpt = Optional.of(logs);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(MeteringLogs.class)).thenReturn(emptyOpt);
        Row row=Struct.create(Row.class);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        Optional opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.single()).thenReturn(row);
        Complaints complaints= Struct.create(Complaints.class);
        complaints.setId("202");
        List<Row> rowvalues1 = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        Optional opt1 = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues1);
        when(result.first()).thenReturn(opt1);
        impl.send();
    }

    @Test(expected = Exception.class)
    public void sendElse1() {
        MeteringLogs logs= Struct.create(MeteringLogs.class);
        logs.setStatus("Failure");
        Optional<MeteringLogs> emptyOpt = Optional.of(logs);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(MeteringLogs.class)).thenReturn(emptyOpt);
        Row row=Struct.create(Row.class);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status", "Failure");
        Optional opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.single()).thenReturn(row);
        Complaints complaints= Struct.create(Complaints.class);
        complaints.setId("202");
        List<Row> rowvalues1 = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        Optional opt1 = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues1);
        when(result.first()).thenReturn(opt1);
        impl.send();
    }


    @Test
    public void send() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        cds.gen.com.sap.ic.cmh.complaint.Complaints complaints= Struct.create(cds.gen.com.sap.ic.cmh.complaint.Complaints.class);
        complaints.setId("202");
        List<cds.gen.com.sap.ic.cmh.complaint.Complaints> result1=new ArrayList<>();
        result1.add(complaints);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(cds.gen.com.sap.ic.cmh.complaint.Complaints.class)).thenReturn((result1));
        when(result.list()).thenReturn(rowValues);
        when(result.iterator()).thenReturn(rowValues.iterator());

        impl.send();
    }
    @Test
    public void updateMeterLogsTest() {
        MeteringLogs logs= Struct.create(MeteringLogs.class);
        logs.setStatus("Failure");
        Optional<MeteringLogs> emptyOpt = Optional.of(logs);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(MeteringLogs.class)).thenReturn(emptyOpt);
        Row row=Struct.create(Row.class);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status", "Failure");
        Optional opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.single()).thenReturn(row);
        cds.gen.com.sap.ic.cmh.complaint.Complaints complaints= Struct.create(cds.gen.com.sap.ic.cmh.complaint.Complaints.class);
        complaints.setId("202");
        List<Row> rowvalues1 = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        row.put("startTime",Instant.now());
        row.put("time",Instant.now());
        Optional opt1 = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues1);
        when(result.first()).thenReturn(opt1);
        when(tenantSubscriptionService.getSaasSubscription()).thenReturn(subscription);
        Subscription sub=new Subscription();
         List<Subscription> list=new ArrayList<>();
        list.add(sub);
         when(subscription.getSubscriptions()).thenReturn(list);
         when(userInfo.getTenant()).thenReturn("t38924hwe895r");
        impl.send();
    }


    @Test
    public void updateMeterLogsElse1Test() {
        MeteringLogs logs= Struct.create(MeteringLogs.class);
        logs.setStatus("Failure");

        Optional<MeteringLogs> emptyOpt = Optional.of(logs);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(MeteringLogs.class)).thenReturn(emptyOpt);
        Row row=Struct.create(Row.class);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status", "Failure");
        Optional opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.single()).thenReturn(row);
        cds.gen.com.sap.ic.cmh.complaint.Complaints complaints= Struct.create(cds.gen.com.sap.ic.cmh.complaint.Complaints.class);
        complaints.setId("202");
        List<Row> rowvalues1 = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        row.put("startTime",Instant.now());
        row.put("time",Instant.now());
        Optional opt1 = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues1);
        when(tenantSubscriptionService.getSaasSubscription()).thenReturn(subscription);
        Subscription sub=new Subscription();
        List<Subscription> list=new ArrayList<>();
        list.add(sub);
        when(subscription.getSubscriptions()).thenReturn(list);
        when(userInfo.getTenant()).thenReturn("t38924hwe895r");
        when(meteringClient.meteringServiceClient(any(),any())).thenReturn(true);
        impl.send();
    }

    @Test
    public void updateMeterLogsElseTest() {
        MeteringLogs logs= Struct.create(MeteringLogs.class);
        logs.setStatus("Failure");

        Optional<MeteringLogs> emptyOpt = Optional.of(logs);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(MeteringLogs.class)).thenReturn(emptyOpt);
        Row row=Struct.create(Row.class);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status", "Failure");
        Optional opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.single()).thenReturn(row);
        cds.gen.com.sap.ic.cmh.complaint.Complaints complaints= Struct.create(cds.gen.com.sap.ic.cmh.complaint.Complaints.class);
        complaints.setId("202");
        List<Row> rowvalues1 = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        row.put("startTime",Instant.now());
        row.put("time",Instant.now());
        Optional opt1 = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues1);
        when(tenantSubscriptionService.getSaasSubscription()).thenReturn(subscription);
        Subscription sub=new Subscription();
        List<Subscription> list=new ArrayList<>();
        list.add(sub);
        when(subscription.getSubscriptions()).thenReturn(list);
        when(userInfo.getTenant()).thenReturn("t38924hwe895r");
        when(meteringClient.meteringServiceClient(any(),any())).thenReturn(true);
        impl.send();
    }

    @Test
    public void updateMeterLogsElse4Test() {
        MeteringLogs logs= Struct.create(MeteringLogs.class);
        logs.setStatus("Failure");
        Optional<MeteringLogs> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(MeteringLogs.class)).thenReturn(emptyOpt);
        Row row=Struct.create(Row.class);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status", "Failure");
        Optional opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.single()).thenReturn(row);
        cds.gen.com.sap.ic.cmh.complaint.Complaints complaints= Struct.create(cds.gen.com.sap.ic.cmh.complaint.Complaints.class);
        complaints.setId("202");
        List<Row> rowvalues1 = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        row.put("startTime",Instant.now());
        row.put("time",Instant.now());
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues1);
        when(tenantSubscriptionService.getSaasSubscription()).thenReturn(subscription);
        Subscription sub=new Subscription();
        List<Subscription> list=new ArrayList<>();
        list.add(sub);
        when(subscription.getSubscriptions()).thenReturn(list);
        when(userInfo.getTenant()).thenReturn("t38924hwe895r");
        when(meteringClient.meteringServiceClient(any(),any())).thenReturn(true);
        impl.send();
    }

}
