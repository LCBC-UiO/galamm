#
rm -r inst/include/autodiff
mkdir tmp
cd tmp
git clone https://github.com/autodiff/autodiff.git

mkdir ../inst/include/autodiff/
cp -r autodiff/autodiff/common ../inst/include/autodiff
cp -r autodiff/autodiff/forward ../inst/include/autodiff
cp -r autodiff/autodiff/reverse ../inst/include/autodiff

cd ..
rm -rf tmp/

R CMD BATCH dev-scripts/insert_sparse_autodiff.R
rm insert_sparse_autodiff.Rout
