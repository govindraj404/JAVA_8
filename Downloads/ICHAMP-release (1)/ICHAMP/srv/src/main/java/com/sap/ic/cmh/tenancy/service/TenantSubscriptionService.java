package com.sap.ic.cmh.tenancy.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.ic.cmh.network.model.ResponseDto;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.tenancy.model.SaasSubscription;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.XsuaaToken;
import io.pivotal.cfenv.core.CfEnv;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("cloud")
public class TenantSubscriptionService {

    private final Logger logger = LoggerFactory.getLogger(TenantSubscriptionService.class);

    @Autowired
    XsuaaToken xsuaa;

    @Autowired
    HttpService httpService;

    /**
     * Method to get saas subscriptions
     *
     * @return SaasSubscription
     * @throws JsonMappingException
     * @throws JsonProcessingException
     */
    public SaasSubscription getSaasSubscription() {
        CfEnv env = new CfEnv();
        Map<String, Object> saasServiceMap = env.findCredentialsByLabel(Constants.SAAS_REGISTRY_SERVICE_LABEL_NAME)
                .getMap();

        String jwtToken = xsuaa.getAccessToken(
                saasServiceMap.get(Constants.URL).toString(),
                saasServiceMap.get(Constants.CLIENT_ID).toString(),
                saasServiceMap.get(Constants.CLIENT_SECRET).toString());
        if (jwtToken != null) {
            String saasRegistryServiceUrl = (String) saasServiceMap.get(Constants.SAAS_REGISTRY_URL);
            try {
                ResponseDto responseDto = httpService.get(
                        saasRegistryServiceUrl + Constants.SAAS_MANAGER,
                        jwtToken,
                        new HashMap<>());
                ObjectMapper om = new ObjectMapper();
                return om.readValue(responseDto.getContent(), SaasSubscription.class);
            } catch (IOException e) {
                LoggerHelper.logExceptionWithMessage(logger, "Exception occurred while getting saas subscriptions", e);
            }
        }
        return null;
    }
}
