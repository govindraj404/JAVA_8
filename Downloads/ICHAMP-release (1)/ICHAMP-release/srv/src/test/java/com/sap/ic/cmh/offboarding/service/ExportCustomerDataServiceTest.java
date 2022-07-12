package com.sap.ic.cmh.offboarding.service;

import cds.gen.com.sap.ic.cmh.customerdataexportstatus.CustomerDataExportStatuses;
import com.sap.cds.Struct;
import com.sap.cds.services.request.UserInfo;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.cds.services.runtime.RequestContextRunner;
import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.objectstore.service.ObjectStoreService;
import com.sap.ic.cmh.offboarding.persistency.CustomerDataExportStatusDao;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

public class ExportCustomerDataServiceTest {
    @InjectMocks
    @Autowired
    ExportCustomerDataService service;
    @Mock
    PlatformUtil platformUtil;
    @Mock
    CustomerDataExportStatusDao customerDataExportStatusDao;
    @Mock
    CdsRuntime cdsRuntime;
    @Mock
    ObjectStoreService objectStoreService;

    @Mock
    RestTemplate template;
    @Mock
    ResponseEntity<OAuthToken> responseEntity;
    @Mock
    RequestContextRunner runner;
    @Mock
    JSONObject jsonObject;

    @Mock
    UserInfo userInfo;

    @Mock
    private Connection mockConnection;

    @Mock
    private Statement mockStatement;

    @Mock
    private ResultSet resultSet;
    JSONArray array;

    @Before
    public void beforeClass() throws SQLException {
        MockitoAnnotations.openMocks(this);
        JSONObject jsonObject1=new JSONObject();
        jsonObject1.put("user","admin");
        jsonObject1.put("password","admin");
        jsonObject1.put("schema","admin");
        jsonObject1.put("clientsecret","admin");
        jsonObject1.put("credentials",jsonObject1);
        jsonObject1.put("sm_url","/admin");
        jsonObject1.put("url","admin");
        JSONObject jsonObject=new JSONObject();
        jsonObject.put("Username","admin");
        jsonObject.put("Password","admin");
        jsonObject.put("clientid","admin");
        jsonObject.put("clientsecret","admin");
        jsonObject.put("credentials",jsonObject1);
        jsonObject.put("sm_url","/admin");
        jsonObject.put("url","admin");
        array = new JSONArray();
        array.put(0,jsonObject);

    }


    @Test(expected = Exception.class)
    public void downloadTenantDataTest() throws IOException, SQLException {
        mockConnection = mock(Connection.class);
        mockStatement = mock(Statement.class);
        resultSet = mock(ResultSet.class);
        CustomerDataExportStatuses statuses= Struct.create(CustomerDataExportStatuses.class);
        statuses.setStatus("test1");
        statuses.setId("123");
        JSONObject jObject=new JSONObject();
        jsonObject.put("credentials","admin");
        when(cdsRuntime.requestContext()).thenReturn(runner);
        when(runner.privilegedUser()).thenReturn(runner);
        when(runner.modifyUser(any())).thenReturn(runner);
        when(jsonObject.getJSONObject(any())).thenReturn(jObject);

        mockStatic(DriverManager.class);
        when(DriverManager.getConnection(anyString(), anyString(), anyString())).thenReturn(mockConnection);
        when(mockConnection.createStatement()).thenReturn(mockStatement);
        when(mockStatement.executeQuery(anyString())).thenReturn(resultSet);
        try (MockedConstruction<FileOutputStream> mocked = Mockito.mockConstruction(FileOutputStream.class,
                (mock, context) -> {
                    doNothing().when(mock).write(any());
                })) {

            service.downloadTenantData("subdomain",array,"text_",statuses);
        };

    }


    @Test
    public void getHandlerTest(){
        CustomerDataExportStatuses statuses= Struct.create(CustomerDataExportStatuses.class);
        statuses.setStatus("test1");
        statuses.setId("123");
        when(jsonObject.getJSONObject(any())).thenReturn(jsonObject);
        when(runner.privilegedUser()).thenReturn(runner);
        when(runner.modifyUser(any())).thenReturn(runner);
        service.getHandler(jsonObject);
    }


    @Test
    public void getTenantDBContainersTest(){
        CustomerDataExportStatuses statuses= Struct.create(CustomerDataExportStatuses.class);
        statuses.setStatus("test1");
        statuses.setId("123");
        JSONObject jsonObject=new JSONObject();
        jsonObject.put("Username","admin");
        jsonObject.put("Password","admin");
        jsonObject.put("clientid","admin");
        jsonObject.put("clientsecret","admin");
        jsonObject.put("url","admin");
        jsonObject.put("sm_url","/admin");
        when(platformUtil.getCredentials(any())).thenReturn(jsonObject);
        when(runner.modifyUser(any())).thenReturn(runner);
        when(runner.user(any())).thenReturn(runner);
        try (MockedConstruction<RestTemplate> mocked = Mockito.mockConstruction(RestTemplate.class,
                (mock, context) -> {
                    String json = "{\n"
                            + "\"specversion\" : \"1.0\",\n"
                            + "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
                            + "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
                            + "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n"
                            + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
                            + "\"data\" :\n"
                            + "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F2\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"\",\"CHGNR\":\"001\"},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"\",\"CHGNR\":\"001\"}]}]}]}\n"
                            + "}";
                    ResponseEntity<String> responseStringEntity = new ResponseEntity<String>(json, HttpStatus.ACCEPTED);
                    ResponseEntity<Object> responseEntity = new ResponseEntity<Object>(new OAuthToken(), HttpStatus.ACCEPTED);
                    when(mock.getForEntity(anyString(), Matchers.any())).thenReturn(responseEntity);
                    when(mock.exchange(
                                    anyString(),
                                    Matchers.any(HttpMethod.class),
                                    Matchers.<HttpEntity<?>> any(),
                                    Matchers.<Class<String>> any()
                            )
                    ).thenReturn(responseStringEntity);
                })) {

            service.getTenantDBContainers("jsonObject");
        }


    }

}
