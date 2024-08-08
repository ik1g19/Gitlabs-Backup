import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SmartCardTest {

	@Test
	@DisplayName("Test staff and student boolean")
	void testName() {
		SmartCard cardStudent = new SmartCard("Anna Undergrad");
		assertEquals(cardStudent.getOwner(),"Anna Undergrad", "Tests that getOwner returns the owner");
	}

	@Test
	@DisplayName("Test student boolean")
	void testStaff() {
		SmartCard cardStudent = new SmartCard("Anna Undergrad");

		assertFalse(cardStudent.isStaff(), "Tests whether a student card is a marked as a student");
	}
	
	@Test
	@DisplayName("Test staff boolean")
	void testStudent() {
		SmartCard cardStaff = new SmartCard("Dr. Bob Lecturer");
		cardStaff.setStaff(true);
		
		assertTrue(cardStaff.isStaff(), "Tests whether a staff card is a marked as staff");
	}
}
	
