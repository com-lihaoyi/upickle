package upickle.jsonschema

import utest.*
import utest.framework.GoldenFix
import java.nio.file.Path

object SchemaSnapshotTestUtils {
  private def goldenPath(resourcePath: String): Path = {
    val root = sys.env.getOrElse("MILL_TEST_RESOURCE_DIR", {
      throw new IllegalArgumentException("MILL_TEST_RESOURCE_DIR is not set")
    })
    val path = Path.of(root).resolve(resourcePath)
    if (!java.nio.file.Files.exists(path)) {
      throw new IllegalArgumentException(s"Missing golden resource: $path")
    }
    path
  }

  def assertSchemaSnapshot[T](resourcePath: String)(using JsonSchema[T], GoldenFix.Reporter): Unit = {
    val rendered = JsonSchema.schemaFor[T](upickle.default).render(indent = 2)
    assertGoldenFile(rendered, goldenPath(resourcePath))
  }
}
