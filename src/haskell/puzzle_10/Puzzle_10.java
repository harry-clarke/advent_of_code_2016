package haskell.puzzle_10;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class Puzzle_10 {

	private static final String FILE_NAME = "/Users/harry/advent_of_code_2016/src/haskell/puzzle_10/input.txt";

	private static Pattern BOT_PATTERN = Pattern.compile("bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (output|bot) ([0-9]+)");

	final HashMap<ID, Bot> bots;

	public Puzzle_10() {
		this(FILE_NAME);
	}

	public Puzzle_10(final String file) {
		bots = new HashMap<>();
		parse(file);
	}

	interface Output {
		void take(int val);
	}

	private void parse(final String fileName) {

		Map<Integer, ID> initials = new HashMap<>();

		try (final Stream<String> stream = Files.lines(Paths.get(fileName))) {

			stream.forEach(line -> {
			});

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	class Bot implements Output {

		final ID lowerBot;
		final ID higherBot;
		int val1 = -1;

		int val2 = -1;
		public Bot(ID lowerBot, ID higherBot) {
			this.lowerBot = lowerBot;
			this.higherBot = higherBot;
		}

		private void run() {
			if (val1 == -1 || val2 == -1)
				throw new IllegalStateException();

			if (val1 > val2) {
				bots.get(higherBot).take(val1);
				bots.get(lowerBot).take(val2);
			} else {
				bots.get(higherBot).take(val2);
				bots.get(lowerBot).take(val1);
			}

			val2 = -1;
			val1 = -1;
		}

		public void take(final int val) {
			if (val1 == -1) {
				val1 = val;
			} else if (val2 == -1) {
				val2 = val;
				run();
			}
		}

	}
}

class ID {
	final Type type;
	final int id;

	public ID(Type type, int id) {
		this.type = type;
		this.id = id;
	}
}

enum Type { Output, Bot }
