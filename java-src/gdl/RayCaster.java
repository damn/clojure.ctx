package gdl;

import java.util.ArrayList;

public class RayCaster {

	public static class Step {
		public final int x;
		public final int y;

		public Step(int x, int y) {
			this.x = x;
			this.y = y;
		}

	}

	private final static double EPSILON = 0.00001;

	/**
	 * Returns true if two doubles are considered equal. Tests if the absolute
	 * difference between two doubles has a difference less then EPSILON.
	 *
	 * @param a
	 *            double to compare.
	 * @param b
	 *            double to compare.
	 * @return true true if two doubles are considered equal.
	 */
	private static boolean equals(double a, double b) {
		return a == b ? true : Math.abs(a - b) < EPSILON;
	}

	/**
	 * Adds a little bit (ulp) so that for example 64.0 becomes 65.0 when using
	 * ceil instead of Math.ceil(64.0) returning 64.0.
	 *
	 * @param n
	 *            the number
	 * @return 1 integer higher.
	 */
	private static double nextUp(double n) {
		double ulp = Math.ulp(n);

		return Math.ceil(n + ulp);
	}

	/**
	 * u(start) + t*v(vektor) Stück für Stück wird t bis zum targettile
	 * durchgegangen (erhöht bis zum nächsten step in abh. von v jeweils) und
	 * nebenbei x oder y um 1 step erhöht.
	 *
	 * target tile darf nicht blockiert sein.
	 *
	 * performance verb: if start==Target sofort testen nicht zweimal tmax abs
	 * berechnen und vergleichen -> einmal vorberechnen
	 *
	 * @param startX
	 * @param startY
	 * @param targetX
	 * @param targetY
	 * @return -1 falls max-steps erreicht, falls geblockt werden die steps die
	 *         der caster zurückgelegt hat zurückgegeben.
	 */
	public static int castMaxSteps(double startX, double startY,
			double vectorX, double vectorY, int mapWidth, int mapHeight,
			boolean[][] grid, int maxSteps) {

		double ux = startX;
		double uy = startY;

		int stepX, stepY;

		// Math.signum? -> ne er muss entweder incr oder decr

		if (vectorX < 0) {
			stepX = -1;
		} else {
			stepX = 1;
		}

		if (vectorY < 0) {
			stepY = -1;
		} else {
			stepY = 1;
		}

		double crossFirstBoundaryX, crossFirstBoundaryY;
		if (stepX > 0) {
			crossFirstBoundaryX = nextUp(ux);
		} else {
			crossFirstBoundaryX = Math.floor(ux);
		}

		if (stepY > 0) {
			crossFirstBoundaryY = nextUp(uy);
		} else {
			crossFirstBoundaryY = Math.floor(uy);
		}

		double tMaxX = (double) Math.abs((crossFirstBoundaryX - ux) / vectorX);

		double tMaxY = (double) Math.abs((crossFirstBoundaryY - uy) / vectorY);

		double tileWidth = 1.0f;
		double tileHeight = 1.0f;

		double tDeltaX = stepX * tileWidth / vectorX;
		double tDeltaY = stepY * tileHeight / vectorY;

		double currentX = startX;
		double currentY = startY;

		int steps = 1;

		if (maxSteps == steps) {
			return -1;
		}

		while ((currentX > 0 && currentX < mapWidth && currentY > 0 && currentY < mapHeight)) {

			steps++;

			if (maxSteps == steps) {
				return -1;
			}

			double abstMaxX = Math.abs(tMaxX);
			double abstMaxY = Math.abs(tMaxY);

			// falls tMaxX == tMaxY an einer "ecke" angekommen
			// -> mach step in beide richtungen
			if (equals(abstMaxX, abstMaxY)) {

				// check xstep

				int currentTileX = (int) (currentX + stepX);
				int currentTileY = (int) currentY;

				if (grid[currentTileX][currentTileY]) {
					return steps;
				}

				// check ystep

				currentTileX = (int) currentX;
				currentTileY = (int) (currentY + stepY);

				if (grid[currentTileX][currentTileY]) {
					return steps;
				}

				// two steps
				tMaxX = tMaxX + tDeltaX;
				tMaxY = tMaxY + tDeltaY;

				currentX += stepX;
				currentY += stepY;

			} else if (abstMaxX < abstMaxY) {
				tMaxX = tMaxX + tDeltaX;
				currentX += stepX;
			} else if (abstMaxY < abstMaxX) {
				tMaxY = tMaxY + tDeltaY;
				currentY += stepY;
			}

			int currentTileX = (int) currentX;
			int currentTileY = (int) currentY;

			if (grid[currentTileX][currentTileY]) {
				return steps;
			}

		}

		return -2;
	}

	/**
	 * u(start) + t*v(vektor) Stück für Stück wird t bis zum targettile
	 * durchgegangen (erhöht bis zum nächsten step in abh. von v jeweils) und
	 * nebenbei x oder y um 1 step erhöht.
	 *
	 * target tile darf nicht blockiert sein.
	 *
	 * performance verb: if start==Target sofort testen nicht zweimal tmax abs
	 * berechnen und vergleichen -> einmal vorberechnen
	 *
	 * @param startX
	 * @param startY
	 * @param targetX
	 * @param targetY
	 * @return steplist der gesteppten tiles
	 */
	public static ArrayList<Step> castSteplist(double startX, double startY,
			double targetX, double targetY, int mapWidth, int mapHeight,
			boolean[][] grid) {

		double ux = startX;
		double uy = startY;

		// falsl target = start -> wird infinity -> nur bewegung auf 1 achse
		// natürlich
		double vectorX = (targetX - startX);
		double vectorY = (targetY - startY);

		int stepX, stepY;

		// Math.signum? -> ne er muss entweder incr oder decr

		if (vectorX < 0) {
			stepX = -1;
		} else {
			stepX = 1;
		}

		if (vectorY < 0) {
			stepY = -1;
		} else {
			stepY = 1;
		}

		double crossFirstBoundaryX, crossFirstBoundaryY;
		if (stepX > 0) {
			crossFirstBoundaryX = nextUp(ux);
		} else {
			crossFirstBoundaryX = Math.floor(ux);
		}

		if (stepY > 0) {
			crossFirstBoundaryY = nextUp(uy);
		} else {
			crossFirstBoundaryY = Math.floor(uy);
		}

		double tMaxX = (double) Math.abs((crossFirstBoundaryX - ux) / vectorX);

		double tMaxY = (double) Math.abs((crossFirstBoundaryY - uy) / vectorY);

		double tileWidth = 1.0f;
		double tileHeight = 1.0f;

		double tDeltaX = stepX * tileWidth / vectorX;
		double tDeltaY = stepY * tileHeight / vectorY;

		double currentX = startX;
		double currentY = startY;

		int startTileX = (int) startX;
		int startTileY = (int) startY;
		int targetTileX = (int) targetX;
		int targetTileY = (int) targetY;

		ArrayList<Step> stepList = new ArrayList<Step>();

		stepList.add(new Step(startTileX, startTileY));

		if ((startTileX == targetTileX) && (startTileY == targetTileY)) {
			return stepList;
		}

		while ((currentX > 0 && currentX < mapWidth && currentY > 0 && currentY < mapHeight)) {

			double abstMaxX = Math.abs(tMaxX);
			double abstMaxY = Math.abs(tMaxY);

			// falls tMaxX == tMaxY an einer "ecke" angekommen
			// -> mach step in beide richtungen
			if (equals(abstMaxX, abstMaxY)) {

				// check xstep

				int currentTileX = (int) (currentX + stepX);
				int currentTileY = (int) currentY;

				stepList.add(new Step(currentTileX, currentTileY));

				if (grid[currentTileX][currentTileY]) {
					return stepList;
				}

				// nicht geblockt und target erreicht -> break.
				if ((currentTileX == targetTileX)
						&& (currentTileY == targetTileY)) {
					return stepList;
				}

				// check ystep

				currentTileX = (int) currentX;
				currentTileY = (int) (currentY + stepY);

				stepList.add(new Step(currentTileX, currentTileY));

				if (grid[currentTileX][currentTileY]) {
					return stepList;
				}

				// nicht geblockt und target erreicht -> break.
				if ((currentTileX == targetTileX)
						&& (currentTileY == targetTileY)) {
					return stepList;
				}

				// two steps
				tMaxX = tMaxX + tDeltaX;
				tMaxY = tMaxY + tDeltaY;

				currentX += stepX;
				currentY += stepY;

			} else if (abstMaxX < abstMaxY) {
				tMaxX = tMaxX + tDeltaX;
				currentX += stepX;
			} else if (abstMaxY < abstMaxX) {
				tMaxY = tMaxY + tDeltaY;
				currentY += stepY;
			}

			int currentTileX = (int) currentX;
			int currentTileY = (int) currentY;

			stepList.add(new Step(currentTileX, currentTileY));

			if (grid[currentTileX][currentTileY]) {
				return stepList;
			}

			// nicht geblockt und target erreicht -> break.
			if ((currentTileX == targetTileX) && (currentTileY == targetTileY)) {
				return stepList;
			}
		}

		return stepList;
	}

	/**
	 * u(start) + t*v(vektor) Stück für Stück wird t bis zum targettile
	 * durchgegangen (erhöht bis zum nächsten step in abh. von v jeweils) und
	 * nebenbei x oder y um 1 step erhöht.
	 *
	 * performance verb: if start==Target sofort testen nicht zweimal tmax abs
	 * berechnen und vergleichen -> einmal vorberechnen
	 *
	 * @param startX
	 * @param startY
	 * @param targetX
	 * @param targetY
	 * @return True = geblockt false = frei.
	 */
	public static boolean rayBlocked(double startX, double startY,
			double targetX, double targetY, int mapWidth, int mapHeight,
			boolean[][] grid) {

		double ux = startX;
		double uy = startY;

		// falsl target = start -> wird infinity -> nur bewegung auf 1 achse
		// natürlich
		double vectorX = (targetX - startX);
		double vectorY = (targetY - startY);

		int stepX, stepY;

		// Math.signum? -> ne er muss entweder incr oder decr

		if (vectorX < 0) {
			stepX = -1;
		} else {
			stepX = 1;
		}

		if (vectorY < 0) {
			stepY = -1;
		} else {
			stepY = 1;
		}

		boolean isHorizontalStepXPositive = stepX == 1 && vectorY == 0;
		boolean isVerticalStepYPositive = stepY == 1 && vectorX == 0;

		double crossFirstBoundaryX, crossFirstBoundaryY;
		if (stepX > 0) {
			crossFirstBoundaryX = nextUp(ux);
		} else {
			crossFirstBoundaryX = Math.floor(ux);
		}

		if (stepY > 0) {
			crossFirstBoundaryY = nextUp(uy);
		} else {
			crossFirstBoundaryY = Math.floor(uy);
		}

		double tMaxX = (double) Math.abs((crossFirstBoundaryX - ux) / vectorX);

		double tMaxY = (double) Math.abs((crossFirstBoundaryY - uy) / vectorY);

		double tileWidth = 1.0f;
		double tileHeight = 1.0f;

		double tDeltaX = stepX * tileWidth / vectorX;
		double tDeltaY = stepY * tileHeight / vectorY;

		double currentX = startX;
		double currentY = startY;

		boolean blocked = false;

		int startTileX = (int) startX;
		int startTileY = (int) startY;
		int targetTileX = (int) targetX;
		int targetTileY = (int) targetY;

		boolean targetIsCorner = (targetX == targetTileX && targetY == targetTileY);

		if ((startTileX == targetTileX) && (startTileY == targetTileY)) {

			return grid[targetTileX][targetTileY];
		}

		while ((currentX > 0 && currentX < mapWidth && currentY > 0 && currentY < mapHeight)) {

			double abstMaxX = Math.abs(tMaxX);
			double abstMaxY = Math.abs(tMaxY);

			// falls tMaxX == tMaxY an einer "ecke" angekommen
			// -> mach step in beide richtungen
			if (equals(abstMaxX, abstMaxY)) {

				// touched exactly the corner -> do not check the 3 tiles
				// around
				// the corner for blocked
				if (targetIsCorner) {

					// 1. berechne corner-tile
					int cornerTileX = 0;
					int cornerTileY = 0;

					if (stepX == -1 && stepY == 1) {
						// 1. fall ray kommt von oben rechts
						// step -1/1

						// corner-tile = stepY
						cornerTileX = (int) currentX;
						cornerTileY = (int) (currentY + stepY);

					} else if (stepX == 1 && stepY == 1) {
						// 2. fall ray kommt von oben links
						// step 1/1 = corner tile

						// corner-tile = stepX und stepY
						cornerTileX = (int) (currentX + stepX);
						cornerTileY = (int) (currentY + stepY);

					} else if (stepX == 1 && stepY == -1) {
						// 3. fall ray kommt von unten links
						// step 1/-1 = corner tile

						// corner-tile = stepX
						cornerTileX = (int) (currentX + stepX);
						cornerTileY = (int) currentY;

					} else if (stepX == -1 && stepY == -1) {
						// 4. fall ray kommt von unten rechts
						// step -1/-1

						// corner-tile = current to integer
						cornerTileX = (int) currentX;
						cornerTileY = (int) currentY;
					}

					if (cornerTileX == targetTileX
							&& cornerTileY == targetTileY) {
						break;
					}

				} else {
					// target is not corner -> check all 3 touched tiles for
					// collision.
				}

				// check xstep

				int currentTileX = (int) (currentX + stepX);
				int currentTileY = (int) currentY;

				if (grid[currentTileX][currentTileY]) {
					blocked = true;
					break;
				}

				// nicht geblockt und target erreicht -> break.
				if ((currentTileX == targetTileX)
						&& (currentTileY == targetTileY)) {
					break;
				}

				// check ystep

				currentTileX = (int) currentX;
				currentTileY = (int) (currentY + stepY);

				if (grid[currentTileX][currentTileY]) {
					blocked = true;
					break;
				}

				// nicht geblockt und target erreicht -> break.
				if ((currentTileX == targetTileX)
						&& (currentTileY == targetTileY)) {
					break;
				}

				// two steps
				tMaxX = tMaxX + tDeltaX;
				tMaxY = tMaxY + tDeltaY;

				currentX += stepX;
				currentY += stepY;

			} else if (abstMaxX < abstMaxY) {
				tMaxX = tMaxX + tDeltaX;
				currentX += stepX;
			} else if (abstMaxY < abstMaxX) {
				tMaxY = tMaxY + tDeltaY;
				currentY += stepY;
			}

			int currentTileX = (int) currentX;
			int currentTileY = (int) currentY;

			boolean targetReached = (currentTileX == targetTileX)
					&& (currentTileY == targetTileY);

			// special case der ray ist vertikal oder horizontal und genau die
			// ecke getroffen
			if (targetIsCorner && targetReached
					&& (isHorizontalStepXPositive || isVerticalStepYPositive)) {
				break;
			}

			if (grid[currentTileX][currentTileY]) {
				blocked = true;
				break;
			}

			// nicht geblockt und target erreicht -> break.
			if (targetReached) {
				break;
			}
		}

		return blocked;
	}
}
