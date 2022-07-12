package com.sap.ic.cmh.complaint.handler;

import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.Streams;
import cds.gen.masterdataservice.ActionPreconditions;
import cds.gen.masterdataservice.Actions;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.complaint.persistency.StreamDao;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.complaint.validation.ComplaintValidation;
import com.sap.ic.cmh.complaint.validation.StreamValidation;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class StreamHandlerTest {
    @InjectMocks
    @Autowired
    StreamHandler handler;
    @Mock
    ComplaintValidation complaintValidation;
    @Mock
    StreamValidation streamValidation;
    @Mock
    ComplaintService complaintService;
    @Mock
    Messages messages;
    @Mock
    PersistenceService db;
    @Mock
    MasterDataService masterDataService;

    @Mock
    StreamService streamService;
    @Mock
    CdsReadEventContext context;
    @Mock
    UserInfo  userInfo;
    @Mock
    Result result;
    @Mock
    Row row;
    @Mock
    UserInfo user;

    private Streams streams1;
    private  java.util.stream.Stream<Streams> streams;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        streams1 = Struct.create(Streams.class);
        streams1.setId("120");
        streams= (Stream<Streams>) Stream.of(streams1);
    }
    @Test
    public void testBeforeStreamsUpdateTest(){
        Streams streamsStream=Struct.create(Streams.class);
        streamsStream.setStatusCode("23");
        streamsStream.setId("202");
        Stream stream=Stream.of(streamsStream);
        Optional<Streams> emptyOpt = Optional.of(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Streams.class)).thenReturn(emptyOpt);
        handler.beforeStreamsUpdate((Stream<Streams>) stream);
    }

    @Test
    public void testAfterStreamsUpdateTest(){
        Streams stream1=Struct.create(Streams.class);
        stream1.setStatusCode("CLSD");
        stream1.setId("101");
        stream1.setParentIDId("111");
        stream1.setIsRelevant(true);
        stream1.setStreamTypeCode("QLTY");
        when(streamService.getStream("101")).thenReturn(result);
        when(result.single(Streams.class)).thenReturn(stream1);
        Optional<Streams> emptyOpt = Optional.of(stream1);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Streams.class)).thenReturn(emptyOpt);
        handler.afterStreamsUpdate(stream1);
    }


    @Test
    public void testBeforeStreamBusinessObjectsPatchTest(){
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("23");
        streamsStream.setId("202");
        handler.beforeStreamBusinessObjectsPatch(streamsStream);
    }

    @Test
    public void testBeforeStreamPatchTest(){
        Streams streamsStream=Struct.create(Streams.class);
        streamsStream.setStatusCode("23");
        streamsStream.setId("202");
        handler.beforeStreamPatch(streamsStream);
    }
    @Test
    public void testBeforeBusinessObjectsUpdateTest(){
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("123");
        streamsStream.setId("1");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        handler.beforeBusinessObjectsUpdate(bObjects);
    }


    @Test(expected = Exception.class)
    public void testAfterBusinessObjectsReadExpTest(){
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("123");
        streamsStream.setId("1");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        Optional<BusinessObjects> emptyOpt = Optional.empty();
        when(result.first(BusinessObjects.class)).thenReturn(emptyOpt);
        when(streamService.getBusinessObject(any(String.class))).thenReturn(result);
        handler.afterBusinessObjectsRead(context,bObjects);
    }

    @Test(expected = Exception.class)
    public void testAfterBusinessObjectsReadTest(){
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("123");
        streamsStream.setId("1");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        Optional<BusinessObjects> emptyOpt = Optional.of(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(BusinessObjects.class)).thenReturn(emptyOpt);
        when(streamService.getBusinessObject(any(String.class))).thenReturn(result);
        when(result.list().get(0)).thenReturn(row);
        handler.afterBusinessObjectsRead(context,bObjects);
    }


   /* @Test
    public void testAfterBusinessObjectsReadTestSuccess(){
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("CLM");
        streamsStream.setId("1");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        Optional<BusinessObjects> emptyOpt = Optional.of(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(streamService.getBusinessObject(any(String.class))).thenReturn(result);
        Optional<Row> op=Optional.of(row);
        when(result.first()).thenReturn(op);
        List<BusinessObjects> listBo=new ArrayList<BusinessObjects>();
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        row.put("businessObjectType_code", "CLM");
        row.put("isRelevant", true);
        row.put("isVisible", true);
        when(result.list()).thenReturn(rowvalues);
        handler.afterBusinessObjectsRead(context,bObjects);
    }*/
    @Test
    public void testIsActionEnabled(){
        List<Actions> actionList=new ArrayList<>();
        Actions act=Struct.create(Actions.class);
        act.setCode("code");
        act.setBusinessObjectTypeCode("CLAIM");
        actionList.add(act);
        List<ActionPreconditions> actionListpre=new ArrayList<>();
        ActionPreconditions actPre=Struct.create(ActionPreconditions.class);
        actPre.setBusinessObjectStatusCode("statucode");
        actPre.setBusinessObjectTypeCode("typeCode");
        actPre.setCodeCode("code");
        actionListpre.add(actPre);
        Mockito.when(streamService.getBusinessObjectsWithStatus(any(String.class), any(String.class), any(String.class))).thenReturn(result);
        handler.isActionEnabled(actionList,actionListpre,"CLAIM","cmpId");
    }

    @Test
    public void afterBusinessObjectsReadTest(){
        UserInfo user = UserInfo.create();
        user.hasRole("QualityNotification.Create");
        when(context.getUserInfo()).thenReturn(user);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("CLM");
        streamsStream.setId("100");
        streamsStream.setComplaint("F001");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        row.put("businessObjectType_code", "CLM");
        row.put("isRelevant", true);
        row.put("isVisible", true);
        when(streamService.getBusinessObject(streamsStream.getId())).thenReturn(result);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.afterBusinessObjectsRead(context,bObjects);
    }
    @Test
    public void afterBusinessObjectsReadQNTest(){
        when(context.getUserInfo()).thenReturn(userInfo);
        user.hasRole("QualityNotification.Create");
        when(context.getUserInfo()).thenReturn(user);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("QN");
        streamsStream.setId("100");
        streamsStream.setComplaint("F001");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        row.put("businessObjectType_code", "QN");
        row.put("isRelevant", true);
        row.put("isVisible", true);
        when(streamService.getBusinessObject(streamsStream.getId())).thenReturn(result);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.afterBusinessObjectsRead(context,bObjects);
    }

    @Test
    public void afterBusinessObjectsReadCLMTest(){
        when(context.getUserInfo()).thenReturn(userInfo);
        Set<String> role = new HashSet<>();
        role.add("QualityNotification.Create");
        when(userInfo.getRoles()).thenReturn(role);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("CLM");
        streamsStream.setId("100");
        streamsStream.setComplaint("F001");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        row.put("businessObjectType_code", "CLM");
        row.put("isRelevant", true);
        row.put("isVisible", true);
        when(streamService.getBusinessObject(streamsStream.getId())).thenReturn(result);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.afterBusinessObjectsRead(context,bObjects);
    }

    @Test
    public void afterBusinessObjectsReadCLMTest1(){
        ActionPreconditions actionPreconditions = Struct.create(ActionPreconditions.class);
        actionPreconditions.setCodeCode("ALM");
        List<ActionPreconditions> actionPreconditionsList = new ArrayList<>();
        actionPreconditionsList.add(actionPreconditions);
        Actions action = Struct.create(Actions.class);
        action.setBusinessObjectTypeCode("ALM");
        List<Actions> actionList = new ArrayList<>();
        actionList.add(action);
        when(context.getUserInfo()).thenReturn(userInfo);
        when(userInfo.hasRole(any(String.class))).thenReturn(true);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("CLM");
        streamsStream.setId("100");
        streamsStream.setComplaint("F001");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        row.put("businessObjectType_code", "CLM");
        row.put("isRelevant", true);
        row.put("isVisible", true);
        when(streamService.getBusinessObject(streamsStream.getId())).thenReturn(result);
        when(streamService.getBusinessObjectsWithStatus(any(), any(), any())).thenReturn(result);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(ActionPreconditions.class)).thenReturn(actionPreconditionsList);
        when(result.listOf(Actions.class)).thenReturn(actionList);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        handler.afterBusinessObjectsRead(context,bObjects);
    }

    @Test
    public void afterBusinessObjectsReadRPOTest(){
        when(context.getUserInfo()).thenReturn(userInfo);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("RPO");
        streamsStream.setId("100");
        streamsStream.setComplaint("F001");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        row.put("businessObjectType_code", "RPO");
        row.put("isRelevant", true);
        row.put("isVisible", true);
        when(streamService.getBusinessObject(streamsStream.getId())).thenReturn(result);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.afterBusinessObjectsRead(context,bObjects);
    }

    @Test
    public void afterBusinessObjectsReadS8DTest(){
        when(context.getUserInfo()).thenReturn(userInfo);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("S8D");
        streamsStream.setId("100");
        streamsStream.setComplaint("F001");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        row.put("businessObjectType_code", "S8D");
        row.put("isRelevant", true);
        row.put("isVisible", true);
        when(streamService.getBusinessObject(any())).thenReturn(result);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.afterBusinessObjectsRead(context,bObjects);
    }

    @Test(expected = Exception.class)
    public void afterBusinessObjectsReadComplantiIdtNullTest(){
        when(context.getUserInfo()).thenReturn(userInfo);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode(null);
        streamsStream.setId("1");
        streamsStream.setComplaint("F001");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        when(streamService.getBusinessObject(streamsStream.getId())).thenReturn(result);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.afterBusinessObjectsRead(context,bObjects);
    }

    @Test
    public void afterBusinessObjectsReadComplantiIdEmptyTest(){
        when(context.getUserInfo()).thenReturn(userInfo);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("");
        streamsStream.setId("100");
        streamsStream.setComplaint("F001");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        row.put("businessObjectType_code", "CLM");
        row.put("isRelevant", true);
        row.put("isVisible", true);
        when(streamService.getBusinessObject(streamsStream.getId())).thenReturn(result);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.afterBusinessObjectsRead(context,bObjects);
    }

    @Test(expected =Exception.class)
    public void afterBusinessObjectsReadBussingtypeNullTest(){
        when(context.getUserInfo()).thenReturn(userInfo);
        BusinessObjects streamsStream=Struct.create(BusinessObjects.class);
        streamsStream.setBusinessObjectTypeCode("");
        streamsStream.setId("1");
        java.util.stream.Stream<BusinessObjects> bObjects=Stream.of(streamsStream);
        List<BusinessObjects> businessObjectsList=new ArrayList<>();
        businessObjectsList.add(streamsStream);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessObjects.class)).thenReturn(businessObjectsList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("complaint", "F001");
        when(streamService.getBusinessObject(streamsStream.getId())).thenReturn(null);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.afterBusinessObjectsRead(context,bObjects);
    }
}