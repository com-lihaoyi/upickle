package upickle.jsonschema

import com.fasterxml.jackson.databind.ObjectMapper
import com.networknt.schema.{JsonSchemaFactory, SpecVersion}
import utest.*
import utest.framework.GoldenFix
import java.nio.file.Path

object SchemaSnapshotTestUtils {
  private val mapper = ObjectMapper()
  private val schemaFactory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V202012)

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

  def assertSchemaSerializationCase[T](
      resourcePath: String,
      value: T,
      expectedSerializedJson: String
  )(using JsonSchema[T], upickle.default.Writer[T], GoldenFix.Reporter): Unit = {
    val renderedSchema = JsonSchema.schemaFor[T](upickle.default).render(indent = 2)
    assertGoldenFile(renderedSchema, goldenPath(resourcePath))

    val serialized = upickle.default.write(value)
    assert(serialized == expectedSerializedJson)

    val schema = schemaFactory.getSchema(mapper.readTree(renderedSchema))
    val validationErrors = schema.validate(mapper.readTree(serialized))
    assert(validationErrors.isEmpty)
  }

  def assertSerializationValidatesSchema[T](
      value: T,
      expectedSerializedJson: String
  )(using JsonSchema[T], upickle.default.Writer[T]): Unit = {
    val renderedSchema = JsonSchema.schemaFor[T](upickle.default).render(indent = 2)
    val serialized = upickle.default.write(value)
    assert(serialized == expectedSerializedJson)

    val schema = schemaFactory.getSchema(mapper.readTree(renderedSchema))
    val validationErrors = schema.validate(mapper.readTree(serialized))
    assert(validationErrors.isEmpty)
  }
}
