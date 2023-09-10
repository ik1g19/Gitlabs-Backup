import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class CardLockTest {
	CardLock lock;
	SmartCard cardA;
	SmartCard cardB;
	
	@BeforeEach
	void setup() {
		lock = new CardLock();
		cardA = new SmartCard("Anna Undergrad");
		cardB = new SmartCard("Dr. Bob Lecturer");
		cardB.setStaff(true);	
	}

	@Nested
	@DisplayName("Tests conditions for last seen")
	class LastSeenTest {
		@Test
		@DisplayName("1 swipe")
		void oneSwipeTest() {
			lock.swipeCard(cardA);
			assertEquals(lock.getLastCardSeen(), cardA, "Tests that the cardlast is seen is correct");
		}
		
		@Test
		@DisplayName("2 swipe")
		void twoSwipeTest() {
			lock.swipeCard(cardA);				
			lock.swipeCard(cardB);
			assertEquals(lock.getLastCardSeen(), cardB, "Tests that the cardlast is seen is correct");
		}
	}
	
	@Nested
	@DisplayName("Tests conditions for isUnLocked")
	class UnlockedTest {
		@Test
		@DisplayName("for student on a newly created CardLock")
		void studentUnlockTest() {
			lock.swipeCard(cardA);
			assertFalse(lock.isUnlocked(), "Tests that the CardLock is locked");
		}
		
		@Test
		@DisplayName("for staff on a newly created CardLock")
		void staffUnlockTest() {
			lock.swipeCard(cardB);
			assertTrue(lock.isUnlocked(), "Tests that the CardLock is unlocked");
		}
	}
		
	@Nested
	@DisplayName("Tests conditions for toggleStudentAccess")
	class ToggleTest {
		@Test
		@DisplayName("should not change staff access")
		void staffUnlockTest() {
			lock.toggleStudentAccess();
			lock.swipeCard(cardB);
			assertTrue(lock.isUnlocked(), "when swiped after a toggle");			
		}
		
		@Test
		@DisplayName("should enable student access")
		void studentUnlockTest() {
			lock.toggleStudentAccess();
			lock.swipeCard(cardA);
			assertTrue(lock.isUnlocked(), "Tests that the CardLock is unlocked");
		}
		
		@Test
		@Disabled
		@DisplayName("should clear lock state")
		void unlockRelockTest() {
			lock.toggleStudentAccess();
			lock.swipeCard(cardA);
			assertTrue(lock.isUnlocked(), "Tests that the CardLock is unlocked");

			lock.toggleStudentAccess();
			assertFalse(lock.isUnlocked(), "Tests that the CardLock is locked");
		}
	}
}
