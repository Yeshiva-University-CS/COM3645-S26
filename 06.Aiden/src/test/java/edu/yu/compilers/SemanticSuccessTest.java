package edu.yu.compilers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

/**
 * Write tests to ensure that you accept valid programs
*/
public class SemanticSuccessTest extends OfficialTest {

    private static final Logger logger = LogManager.getLogger(SemanticSuccessTest.class);

    @BeforeEach
    @Override
    void setUp() {
        super.setUp();
        logger.info("Starting semantic success test");
    }

    @AfterEach
    @Override
    void tearDown() {
        super.tearDown();
        logger.info("Completing semantic success test");
    }

    // Write your tests here.
}
