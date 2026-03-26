package edu.yu.compilers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

/**
 * Write tests to ensure you properly flag invalid programs.
*/
public class SemanticErrorTest extends OfficialTest {

    private static final Logger logger = LogManager.getLogger(SemanticErrorTest.class);

    @BeforeEach
    @Override
    void setUp() {
        super.setUp();
        logger.info("Starting semantic error test");
    }

    @AfterEach
    @Override
    void tearDown() {
        super.tearDown();
        logger.info("Completing semantic error test");
    }

    // Write your tests here.
}
