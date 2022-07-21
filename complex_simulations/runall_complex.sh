dos2unix ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V111',g0='g1',h0='h1',f0=0.02,sims=10 --job-name=V111 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V112',g0='g1',h0='h1',f0=0.15,sims=10 --job-name=V112 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V113',g0='g1',h0='h1',f0=0.35,sims=10 --job-name=V113 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V121',g0='g1',h0='h2',f0=0.02,sims=10 --job-name=V121 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V122',g0='g1',h0='h2',f0=0.15,sims=10 --job-name=V122 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V123',g0='g1',h0='h2',f0=0.35,sims=10 --job-name=V123 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V131',g0='g1',h0='h3',f0=0.02,sims=10 --job-name=V131 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V132',g0='g1',h0='h3',f0=0.15,sims=10 --job-name=V132 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V133',g0='g1',h0='h3',f0=0.35,sims=10 --job-name=V133 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V211',g0='g2',h0='h1',f0=0.02,sims=10 --job-name=V211 -t 4:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V212',g0='g2',h0='h1',f0=0.15,sims=10 --job-name=V212 -t 4:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V213',g0='g2',h0='h1',f0=0.35,sims=10 --job-name=V213 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V221',g0='g2',h0='h2',f0=0.02,sims=10 --job-name=V221 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V222',g0='g2',h0='h2',f0=0.15,sims=10 --job-name=V222 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V223',g0='g2',h0='h2',f0=0.35,sims=10 --job-name=V223 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V231',g0='g2',h0='h3',f0=0.02,sims=10 --job-name=V231 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V232',g0='g2',h0='h3',f0=0.15,sims=10 --job-name=V232 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V233',g0='g2',h0='h3',f0=0.35,sims=10 --job-name=V233 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V311',g0='g3',h0='h1',f0=0.02,sims=10 --job-name=V311 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V312',g0='g3',h0='h1',f0=0.15,sims=10 --job-name=V312 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V313',g0='g3',h0='h1',f0=0.35,sims=10 --job-name=V313 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V321',g0='g3',h0='h2',f0=0.02,sims=10 --job-name=V321 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V322',g0='g3',h0='h2',f0=0.15,sims=10 --job-name=V322 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V323',g0='g3',h0='h2',f0=0.35,sims=10 --job-name=V323 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh

sbatch --export=c_name='V331',g0='g3',h0='h3',f0=0.02,sims=10 --job-name=V331 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V332',g0='g3',h0='h3',f0=0.15,sims=10 --job-name=V332 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
sbatch --export=c_name='V333',g0='g3',h0='h3',f0=0.35,sims=10 --job-name=V333 -t 2:00:00 --array=1-100 ~/VT/Complex/Scripts/run_vt_sim_Complex.sh
