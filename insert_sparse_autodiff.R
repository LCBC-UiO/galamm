library(readr)
library(stringr)

ff <- read_lines("inst/include/autodiff/common/eigen.hpp")

ind1 <- str_which(ff, "#include <Eigen/Core>")

ff <- c(ff[1:ind1], "#include <Eigen/SparseCore>",
        ff[(ind1 + 1):length(ff)])

ind2 <- min(str_which(ff, "EIGEN_VERSION_AT_LEAST")) - 1

ff <- c(ff[1:ind2],
        "template<typename Scalar, int Options, typename StorageIndex>",
        "struct VectorTraits<Eigen::SparseMatrix<Scalar, Options, StorageIndex>>",
        "{",
        "  using ValueType = Scalar;",
        "  template<typename NewValueType>",
        "  using ReplaceValueType = Eigen::SparseMatrix<NewValueType, Options, StorageIndex>;",
        "};",
        "",
        ff[(ind2 + 1):length(ff)])

write_lines(ff, "inst/include/autodiff/common/eigen.hpp")
