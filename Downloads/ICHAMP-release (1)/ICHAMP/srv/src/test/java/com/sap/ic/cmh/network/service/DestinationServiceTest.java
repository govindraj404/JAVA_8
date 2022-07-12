package com.sap.ic.cmh.network.service;

import com.sap.cds.services.ServiceException;
import com.sap.cloud.sdk.cloudplatform.connectivity.Destination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.cloud.sdk.cloudplatform.connectivity.exception.DestinationAccessException;
import com.sap.cloud.sdk.cloudplatform.connectivity.exception.DestinationNotFoundException;
import io.vavr.control.Try;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;

import static org.mockito.ArgumentMatchers.any;

public class DestinationServiceTest {
	@InjectMocks
	@Autowired
	DestinationService destinationService;
	@Mock
	ScpCfDestinationLoader scpCfDestinationLoader;
	@Mock
	Try<Destination> destinationTry;
	@Mock
	Destination destination;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
	}

	@Test
	public void testGetRfcDestination() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenReturn(destination);
		destinationService.getRfcDestination(scpCfDestinationLoader, "");
	}

	@Test(expected = ServiceException.class)
	public void testGetRfcDestinationIllegalArgumentException() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenReturn(destination);
		Mockito.when(destination.asRfc()).thenThrow(IllegalArgumentException.class);
		destinationService.getRfcDestination(scpCfDestinationLoader, "");
	}

	@Test
	public void testGetHttpDestination() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenReturn(destination);
		destinationService.getHttpDestination(scpCfDestinationLoader, "");
	}

	@Test(expected = ServiceException.class)
	public void testGetHttpDestinationIllegalArgumentException() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenReturn(destination);
		Mockito.when(destination.asHttp()).thenThrow(IllegalArgumentException.class);
		destinationService.getHttpDestination(scpCfDestinationLoader, "");
	}

	@Test
	public void testGetDestinationOptions() {
		destinationService.getDestinationOptions();
	}

	@Test
	public void testGetDestination() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenReturn(destination);
		destinationService.getDestination(scpCfDestinationLoader, "");
	}

	@Test
	public void testGetAllDestination() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenReturn(destination);
		ArrayList<ScpCfDestination> listScpDestination = new ArrayList<>();
		Iterable<ScpCfDestination> itrIterable = listScpDestination;
		Try<Iterable<ScpCfDestination>> itr = Try.success(itrIterable);
		destinationService.getallDestination(scpCfDestinationLoader);
	}

	@Test
	public void testGetAllDestinationException() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenThrow(DestinationNotFoundException.class);
		ArrayList<ScpCfDestination> listScpDestination = new ArrayList<>();
		Iterable<ScpCfDestination> itrIterable = listScpDestination;
		Try<Iterable<ScpCfDestination>> itr = Try.success(itrIterable);
		destinationService.getallDestination(scpCfDestinationLoader);
	}
	@Test
	public void testGetAllDestinationException_() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenThrow(DestinationAccessException.class);
		ArrayList<ScpCfDestination> listScpDestination = new ArrayList<>();
		Iterable<ScpCfDestination> itrIterable = listScpDestination;
		Try<Iterable<ScpCfDestination>> itr = Try.success(itrIterable);
		destinationService.getallDestination(scpCfDestinationLoader);
	}

	@Test(expected = ServiceException.class)
	public void testGetDestinationDestinationNotFoundException() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenThrow(DestinationNotFoundException.class);
		destinationService.getDestination(scpCfDestinationLoader, "");
	}

	@Test(expected = ServiceException.class)
	public void testGetDestinationDestinationAccessException() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenThrow(DestinationAccessException.class);
		destinationService.getDestination(scpCfDestinationLoader, "");
	}

	@Test(expected = Exception.class)
	public void testReadAllDestination() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(),any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenReturn(destination);
		ArrayList<ScpCfDestination> listScpDestination = new ArrayList<>();
		Iterable<ScpCfDestination> itrIterable = listScpDestination;
		Try<Iterable<ScpCfDestination>> itr = Try.success(itrIterable);
		destinationService.readAllDestination(scpCfDestinationLoader);
	}
}