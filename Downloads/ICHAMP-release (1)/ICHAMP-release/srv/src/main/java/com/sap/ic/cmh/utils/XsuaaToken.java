package com.sap.ic.cmh.utils;

import com.sap.cloud.security.xsuaa.client.ClientCredentials;
import com.sap.cloud.security.xsuaa.client.DefaultOAuth2TokenService;
import com.sap.cloud.security.xsuaa.client.XsuaaDefaultEndpoints;
import com.sap.cloud.security.xsuaa.token.SpringSecurityContext;
import com.sap.cloud.security.xsuaa.tokenflows.TokenFlowException;
import com.sap.cloud.security.xsuaa.tokenflows.XsuaaTokenFlows;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class XsuaaToken {

    private static final Logger logger = LoggerFactory.getLogger(com.sap.ic.cmh.utils.XsuaaToken.class);

    /**
     * Generate jwt using client credential flow
     *
     * @param oAuthURL
     * @param clientId
     * @param clientSecret
     * @return
     */
    public String getAccessToken(String oAuthURL, String clientId, String clientSecret) {
        String jwt = null;
        XsuaaTokenFlows tokenFlows =
                new XsuaaTokenFlows(
                        new DefaultOAuth2TokenService(),
                        new XsuaaDefaultEndpoints(oAuthURL),
                        new ClientCredentials(clientId, clientSecret));

        try {
            jwt = tokenFlows.clientCredentialsTokenFlow().execute().getAccessToken();
        } catch (TokenFlowException e) {
            LoggerHelper.logExceptionWithMessage(logger, "Exception occurred while generating jwt", e);
        }
        return jwt;
    }

     /**
     * Get the subdomain
     */
	public String getSubdomain() {
		return SpringSecurityContext.getToken().getSubdomain();
	}
}
