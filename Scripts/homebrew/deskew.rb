# Homebrew formula for Deskew CLI.
# Kept in the Deskew repository (Scripts/homebrew/deskew.rb) and published
# unchanged as Formula/deskew.rb of the tap https://gitlab.com/deskew/homebrew
class Deskew < Formula
  desc "Command-line tool for straightening scanned text documents"
  homepage "https://galfar.vevb.net/deskew"
  # New release: update the tag and checksum (curl -L <url> | shasum -a 256)
  url "https://github.com/galfar/deskew/archive/refs/tags/v1.35.tar.gz"
  sha256 "b57f005d1a193ad829ac25a733c6a2511f2db2facf38334b75a9e80280f973b2"
  license "MPL-2.0"
  head "https://github.com/galfar/deskew.git", branch: "master"

  # No runtime dependencies: TIFF, JPEG, PNG and zlib codecs are compiled in.
  # Deskew uses libtiff, libjpeg-turbo, zlib or libwebp when it finds them
  # at run time (deskew -s l lists them), WebP output needs libwebp.
  depends_on "fpc" => :build

  def install
    system "bash", "Scripts/compile.sh"
    bin.install "Bin/deskew"
    pkgshare.install "TestImages/1-g4.tif"
  end

  test do
    # 1-bit TIFF scan, read and written by the compiled-in TIFF codec
    output = shell_output("#{bin}/deskew -o #{testpath}/out.tif #{pkgshare}/1-g4.tif")
    assert_match(/Skew angle found \[deg\]: -1\.3\d+/, output)
    assert_match "Rotating image", output
    assert_path_exists testpath/"out.tif"

    system bin/"deskew", "-o", testpath/"out.png", test_fixtures("test.png")
    assert_path_exists testpath/"out.png"
  end
end
