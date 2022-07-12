package com.sap.ic.cmh.utils;

import com.sap.ic.cmh.exceptions.PrecompilationException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;

import javax.validation.constraints.NotNull;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class MessageKeyPrecompilerTest {
    String projectPath;
    Path sourcePath;

    @Mock
    private MessageKeyPrecompiler messageKeyPrecompiler;
    @Before
    public void init() throws URISyntaxException {
        projectPath = new File(
                MessageKeyPrecompiler.class.getProtectionDomain().getCodeSource().getLocation().toURI()).getParentFile()
                .getParent();
        sourcePath = Paths.get(projectPath, MessageKeyPrecompiler.I18N_FILENAME);
    }

    @Test
    public void testEnsureTargetPathExists() throws PrecompilationException {
        final Path targetPath = Paths.get(projectPath, MessageKeyPrecompiler.GEN_FOLDER,
                MessageKeyPrecompiler.MSGKEYS_NAMES_FILENAME);
        final Path targetParentPath = targetPath.getParent();

        MessageKeyPrecompiler.ensureTargetPathExists(targetPath, targetParentPath);

        final Path dummyTargetPath = Paths.get(projectPath);
        try {
            MessageKeyPrecompiler.ensureTargetPathExists(dummyTargetPath, null);
        } catch (final PrecompilationException e) {
            // PrecompilationException
        }

        final Path dummyTargetParentPath = dummyTargetPath.getParent();
        try {
            MessageKeyPrecompiler.ensureTargetPathExists(targetPath, dummyTargetParentPath);
        } catch (final PrecompilationException e) {
            // PrecompilationException
            final boolean success = targetParentPath.toFile().mkdirs();
            when(targetParentPath.toFile().mkdirs()).thenReturn(true);
            when(Paths.get(projectPath, MessageKeyPrecompiler.GEN_FOLDER,
                    MessageKeyPrecompiler.MSGKEYS_NAMES_FILENAME)).thenReturn(targetParentPath);
            when(dummyTargetPath.getParent()).thenReturn(dummyTargetParentPath);
            throw new PrecompilationException(targetParentPath.toAbsolutePath() + " could not be created", null);
        }
    }

    @Test
    public void testCheckI18NFileExistence() throws PrecompilationException {
        MessageKeyPrecompiler.checkI18NFileExistence(sourcePath);

        final Path dummyTargetPath = Paths.get(projectPath, "/dummy");
        try {
            MessageKeyPrecompiler.checkI18NFileExistence(dummyTargetPath);
        } catch (final PrecompilationException e) {
            // PrecompilationException
        }
    }

    @Test
    public void testReadI18nKeys() throws PrecompilationException {
        MessageKeyPrecompiler.readI18nKeys(sourcePath);

        final Path dummyTargetPath = Paths.get(projectPath, "/dummy");
        try {
            MessageKeyPrecompiler.readI18nKeys(dummyTargetPath);
        } catch (final PrecompilationException e) {
            // PrecompilationException
        }
    }

    @Test
    public void testGenerateMessageKeysClass() throws PrecompilationException {
        MessageKeyPrecompiler.generateMessageKeysClass(sourcePath);
    }
    @Test
    public void testMain() throws PrecompilationException, URISyntaxException{
        String[] arg = null;
        final String projectPath = new File(
                MessageKeyPrecompiler.class.getProtectionDomain().getCodeSource().getLocation().toURI()).getParentFile()
                .getParent();

        final Path sourcePath = Paths.get(projectPath, MessageKeyPrecompiler.I18N_FILENAME);
        final Path targetPath = Paths.get(projectPath, MessageKeyPrecompiler.GEN_FOLDER,
                MessageKeyPrecompiler.MSGKEYS_NAMES_FILENAME);
        final Path targetParentPath = targetPath.getParent();
        MessageKeyPrecompiler.checkI18NFileExistence(sourcePath);
        MessageKeyPrecompiler.ensureTargetPathExists(targetPath, targetParentPath);
        final String sourceCode = MessageKeyPrecompiler.generateMessageKeysClass(sourcePath);
    try {
    Files.write(targetPath, sourceCode.getBytes(StandardCharsets.UTF_8));
}
    catch (IOException exception){
        // IOException
    }
        messageKeyPrecompiler.main(arg);

    }

}