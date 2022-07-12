 package com.sap.ic.cmh.network.service;

import java.time.Duration;
import com.sap.cds.services.ErrorStatuses;
import com.sap.cds.services.ServiceException;
import com.sap.cloud.sdk.cloudplatform.connectivity.*;
import com.sap.cloud.sdk.cloudplatform.connectivity.exception.DestinationAccessException;
import com.sap.cloud.sdk.cloudplatform.connectivity.exception.DestinationNotFoundException;
import com.sap.ic.cmh.gen.MessageKeys;
import org.springframework.stereotype.Component;
import io.vavr.control.Try;
import java.util.Iterator;


@Component
public class DestinationService {

    public RfcDestination getRfcDestination(ScpCfDestinationLoader scpCfDestinationLoader, String destinationName) {
        RfcDestination rfcDestination;
        try {
            rfcDestination = getDestination(scpCfDestinationLoader, destinationName).asRfc();
        } catch (IllegalArgumentException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST,
                    MessageKeys.NOT_A_RFC_DESTINATION + ": " + destinationName);
        }

        return rfcDestination;
    }

    public HttpDestination getHttpDestination(ScpCfDestinationLoader scpCfDestinationLoader, String destinationName) {
        HttpDestination httpDestination;
        try {
            httpDestination = getDestination(scpCfDestinationLoader, destinationName).asHttp();
        } catch (IllegalArgumentException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST,
                    MessageKeys.NOT_A_HTTP_DESTINATION);
        }

        return httpDestination;
    }

    public DestinationOptions getDestinationOptions() {
        return DestinationOptions.builder()
                .augmentBuilder(ScpCfDestinationOptionsAugmenter.augmenter().retrievalStrategy(
                        ScpCfDestinationRetrievalStrategy.SUBSCRIBER_THEN_PROVIDER))
                .build();
    }

    public Destination getDestination(ScpCfDestinationLoader scpCfDestinationLoader, String destinationName) {
        Destination destination;
        try {
            final DestinationOptions options = getDestinationOptions();
            destination = scpCfDestinationLoader.tryGetDestination(destinationName, options).get();
        } catch (DestinationNotFoundException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST, MessageKeys.DESTINATION_NOT_FOUND + ": " + destinationName);
        } catch (DestinationAccessException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST,
                    MessageKeys.DESTINATION_ACCESS_FAILED + ": " + destinationName);
        }

        return  destination;
    }

         /**
	 *  Get all destinations from cf 
     * 
	 * @param {@link options} DestinationOptions getDestinationOptions()
	 * 
	 * @public
	 */

    public Try<Iterable<ScpCfDestination>> getallDestination(ScpCfDestinationLoader scpCfDestinationLoader) {
        Try<Iterable<ScpCfDestination>> destinations;
        try {
         DestinationLoader destinationLoader = new ScpCfDestinationLoader(Duration.ofSeconds(10));
         DestinationAccessor.setLoader(destinationLoader);
         final DestinationOptions options = getDestinationOptions();  
         destinations = scpCfDestinationLoader.tryGetAllDestinations(options);
        } catch (DestinationNotFoundException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST, "MessageKeys.DESTINATION_NOT_FOUND");
        } catch (DestinationAccessException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST,
                    "MessageKeys.DESTINATION_ACCESS_FAILED");
        }
        return destinations;
    } 

        public Iterator<ScpCfDestination> readAllDestination(ScpCfDestinationLoader scpCfDestinationLoader) {
        Try<Iterable<ScpCfDestination>> getallDestinations = getallDestination(scpCfDestinationLoader);
        return getallDestinations.get().iterator();

    }
}
 