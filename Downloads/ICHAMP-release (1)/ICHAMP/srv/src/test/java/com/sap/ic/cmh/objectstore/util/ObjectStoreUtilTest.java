package com.sap.ic.cmh.objectstore.util;

import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import org.jclouds.blobstore.BlobStore;
import org.jclouds.blobstore.BlobStoreContext;
import org.jclouds.blobstore.ContainerNotFoundException;
import org.jclouds.blobstore.domain.Blob;
import org.jclouds.blobstore.domain.BlobBuilder;
import org.jclouds.blobstore.domain.BlobMetadata;
import org.jclouds.blobstore.domain.PageSet;
import org.jclouds.blobstore.domain.StorageMetadata;
import org.jclouds.blobstore.domain.MutableBlobMetadata;
import org.jclouds.io.MutableContentMetadata;
import org.jclouds.io.Payload;
import java.net.URI;
import java.net.URISyntaxException;
import java.io.InputStream;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ObjectStoreUtilTest {

    @InjectMocks
    ObjectStoreUtil util;

    @Mock
    PlatformUtil platformUtil;

    @Mock
    JSONObject destinationDetails;
    @Mock
    private BlobMetadata blobMetadata;
    @Mock
    private Blob blob;
    @Mock
    private Payload payload;
    @Mock
    private BlobBuilder blobBuilder;
    @Mock
    private BlobBuilder.PayloadBlobBuilder payloadBlobBuilder;
    @Mock
    private BlobStore blobStore;
    @Mock
    private BlobStoreContext context;
    @Mock
    StorageMetadata metadata;
    @Mock
    MutableBlobMetadata mutableBlobMetadata;
    @Mock
    MutableContentMetadata contentMetadata ;
    @Mock
    URI uri;
    

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetBucket() {
        JSONObject jsonObject = new JSONObject();
        JSONObject uaaObject = new JSONObject();
        uaaObject.put("clientid", "id");
        uaaObject.put("clientsecret", "secret");
        uaaObject.put("uaadomain", "uaadomain");
        jsonObject.put("uaa", uaaObject);
        jsonObject.put("subdomain", "value");
        jsonObject.put("uaadomain", "uaadomain");
        jsonObject.put("clientid", "id");
        jsonObject.put("clientsecret", "secret");
        jsonObject.put("uri", "/secret");
        Mockito.when(platformUtil.getCredentials("jobscheduler")).thenReturn(jsonObject);
        when(platformUtil.getCredentials(any())).thenReturn(jsonObject);
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
                    OAuthToken oAuthToken = new OAuthToken();
                    oAuthToken.setAccessToken("accessToken");
                    oAuthToken.setTokenType("TokenType");
                    oAuthToken.setExpiresIn("expire");
                    Map<String, Object> map = new HashMap<String, Object>();
                    map.put("destinationConfiguration", new Object());
                    ResponseEntity<Map<String, Object>> responseStringEntity = new ResponseEntity<Map<String, Object>>(map,HttpStatus.ACCEPTED);
                    ResponseEntity<Object> responseEntity = new ResponseEntity<Object>(oAuthToken, HttpStatus.ACCEPTED);
                    when(mock.getForEntity(anyString(), Matchers.any())).thenReturn(responseEntity);
                    when(mock.exchange(
                                    anyString(),
                                    Matchers.any(HttpMethod.class),
                                    Matchers.<HttpEntity<?>> any(),
                                    Matchers.<ParameterizedTypeReference<Map<String, Object>>> any()
                            )
                    ).thenReturn(responseStringEntity);
                })) {
            util.getBucket("subdomain");
        }
    }


    @Test(expected = Exception.class)
    public void testGetBucketJsonMappingException() {
        JSONObject jsonObject = new JSONObject();
        JSONObject uaaObject = new JSONObject();
        uaaObject.put("clientid", "id");
        uaaObject.put("clientsecret", "secret");
        uaaObject.put("uaadomain", "uaadomain");
        jsonObject.put("uaa", uaaObject);
        jsonObject.put("subdomain", "value");
        jsonObject.put("uaadomain", "uaadomain");
        jsonObject.put("clientid", "id");
        jsonObject.put("clientsecret", "secret");
        jsonObject.put("uri", "/secret");
        Mockito.when(platformUtil.getCredentials("jobscheduler")).thenReturn(jsonObject);
        when(platformUtil.getCredentials(any())).thenReturn(jsonObject);
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
                    OAuthToken oAuthToken = new OAuthToken();
                    oAuthToken.setAccessToken("accessToken");
                    oAuthToken.setTokenType("TokenType");
                    oAuthToken.setExpiresIn("expire");
                    Map<String, Object> map = new HashMap<String, Object>();
                    map.put("destinationConfiguration", new Object());
                    ResponseEntity<Map<String, Object>> responseStringEntity = new ResponseEntity<Map<String, Object>>(map,HttpStatus.ACCEPTED);
                    ResponseEntity<Object> responseEntity = new ResponseEntity<Object>(oAuthToken, HttpStatus.ACCEPTED);
                    when(mock.getForEntity(anyString(), Matchers.any())).thenReturn(responseEntity);
                    when(mock.exchange(
                                    anyString(),
                                    Matchers.any(HttpMethod.class),
                                    Matchers.<HttpEntity<?>> any(),
                                    Matchers.<ParameterizedTypeReference<Map<String, Object>>> any()
                            )
                    ).thenThrow(RestClientException.class);
                })) {
            util.getBucket("subdomain");
        }
    }
    
    @Test
    public void testReadableSizeEmpty() {
    	util.readableFileSize(0);
    }
    
    @Test
    public void testReadableSize() {
    	util.readableFileSize(125644);
    }
    @Test
    public void testCreateBlob() throws URISyntaxException {
    	Map<String, String> userMetadata = new HashMap<>();
        userMetadata.put("desc", "etag");
    	when(context.getBlobStore()).thenReturn(blobStore);
        when(blobStore.blobBuilder("resource")).thenReturn(blobBuilder);
        when(blobBuilder.payload(any(InputStream.class))).thenReturn(payloadBlobBuilder);
        when(payloadBlobBuilder.contentType("image/png")).thenReturn(payloadBlobBuilder);
        when(payloadBlobBuilder.userMetadata(userMetadata)).thenReturn(blobBuilder);
        when(payloadBlobBuilder.build()).thenReturn(blob);
        when(blobStore.getBlob("container", "resource")).thenReturn(blob);
        when(blob.getMetadata()).thenReturn(mutableBlobMetadata);
        when(mutableBlobMetadata.getETag()).thenReturn("etag");
        when(mutableBlobMetadata.getContainer()).thenReturn("bucket");
        when(mutableBlobMetadata.getUri()).thenReturn(new URI("testFile.png"));
        when(mutableBlobMetadata.getSize()).thenReturn(Long.parseLong("1024"));
        when(mutableBlobMetadata.getLastModified()).thenReturn(new java.util.Date());
        when(blob.getPayload()).thenReturn(payload);
        when(payload.getContentMetadata()).thenReturn(contentMetadata);
        when(contentMetadata.getContentType()).thenReturn("image/png");
        when(mutableBlobMetadata.getUserMetadata()).thenReturn(userMetadata);
        
        util.createBlobFile(blob);
    }
}
