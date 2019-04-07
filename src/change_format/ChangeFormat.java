package change_format;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class ChangeFormat {
	private static final String INPUT = "./output/bm25.out";
	private static final String OUTPUT = "./output/bm25.eval";

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		BufferedReader br = null;
		FileReader fr = null;
		BufferedWriter bw = null;
		FileWriter fw = null;

		try {
			fr = new FileReader(INPUT);
			fw = new FileWriter(OUTPUT);
			br = new BufferedReader(fr);
			bw = new BufferedWriter(fw);

			String sCurrentLine;
			String out = "qid num_ret num_rel num_rel_ret map Rprec bpref recip_rank iprec_at_recall_0.00 iprec_at_recall_0.10"
					+ " iprec_at_recall_0.20 iprec_at_recall_0.30 iprec_at_recall_0.40 iprec_at_recall_0.50 iprec_at_recall_0.60"
					+ " iprec_at_recall_0.70 iprec_at_recall_0.80 iprec_at_recall_0.90 iprec_at_recall_1.00 P_5 P_10 P_15 P_20 P_30 P_100 P_200 P_500 P_1000";
			String id = "";
			while ((sCurrentLine = br.readLine()) != null) {
				String[] data = sCurrentLine.trim().split("\\t");
				if (!id.equals(data[1])) {
					out += "\r\n";
					if (!id.equals("all")) {
						bw.write(out);
					}
					out = data[1];
					id = data[1];
				}
				out += " " + data[2];
			}

		} catch (IOException e) {

			e.printStackTrace();

		} finally {

			try {

				if (br != null)
					br.close();

				if (fr != null)
					fr.close();
				
				if (bw != null)
					bw.close();

				if (fw != null)
					fw.close();

			} catch (IOException ex) {

				ex.printStackTrace();

			}
		}

	}
}
